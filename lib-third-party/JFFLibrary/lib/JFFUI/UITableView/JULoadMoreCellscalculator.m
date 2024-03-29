#import "JULoadMoreCellscalculator.h"

#import "UITableView+WithinUpdates.h"

#undef SHOW_DEBUG_LOGS
#import <JFFLibrary/JDebugLog.h>

@implementation JULoadMoreCellscalculator

@synthesize currentCount       = _current_count       ;
@synthesize pageSize           = _page_size           ;
@synthesize totalElementsCount = _total_elements_count;

@dynamic isPagingDisabled;
@dynamic isPagingEnabled ;
@dynamic numberOfRows    ;

@dynamic hasNoElements;
@dynamic allElementsLoaded;
@dynamic loadMoreIndexPath;

#warning More Unit tests required

static const NSUInteger RIUndefinedElementsCount = NSUIntegerMax;
static const NSUInteger RIPagingDisabled         = 0;


#pragma mark -
#pragma mark Helper conditions
-(BOOL)hasNoElements
{
   return ( self.totalElementsCount == 0  ) || ( self.totalElementsCount == RIUndefinedElementsCount );
}

-(BOOL)allElementsLoaded
{
   return ( self.currentCount >=  self.totalElementsCount );
}

-(BOOL)isPagingDisabled
{
   BOOL result_ = ( RIPagingDisabled == self.pageSize );
   //NSLog( @"result[%d] : %d == %d", result_, RIPagingDisabled, self.pageSize );

   return result_;
}

-(BOOL)isPagingEnabled
{
   return !self.isPagingDisabled;
}

#pragma mark -
#pragma mark Load More
-(NSIndexPath*)loadMoreIndexPath
{
   return [ NSIndexPath indexPathForRow: self.currentCount
                              inSection: 0 ];
}

-(BOOL)isLoadMoreIndexPath:( NSIndexPath* )index_path_
{
   return 
      ( self.isPagingEnabled ) &&
      ( self.currentCount == index_path_.row );
}

-(BOOL)noNeedToLoadElementAtIndex:( NSUInteger )index_
{
   return ( index_ < self.currentCount );
}

-(NSArray*)prepareIndexPathEntriesForBottomCells:(NSUInteger)cells_count_
{
   if ( 0 == cells_count_ )
   {
      return nil;
   }
   
   NSMutableArray* index_paths_ = [ NSMutableArray arrayWithCapacity: cells_count_ ];
   
   NSUInteger new_row_index_ = self.currentCount + 1; //right after LoadMore button.
   for ( int i = 0; i < cells_count_; ++i, ++new_row_index_ )
   {
      NSIndexPath* new_item_ = [ NSIndexPath indexPathForRow: new_row_index_
                                                   inSection: 0 ];
      
      [ index_paths_ addObject: new_item_ ];
   }
   
   return index_paths_;
}

-(NSUInteger)suggestElementsToAddCountForIndexPath:( NSIndexPath* )index_path_
                                   overflowOccured:( BOOL* )out_is_overflow_
{
   return [ self suggestElementsToAddCountForIndex: index_path_.row
                                   overflowOccured: out_is_overflow_ ];
}

-(NSUInteger)suggestElementsToAddCountForIndex:( NSUInteger )index_
                               overflowOccured:( BOOL* )out_is_overflow_
{
   NSAssert( out_is_overflow_, @"is_overflow_ is not optional" );
   *out_is_overflow_ = NO;
   
   // if all loaded
   if ( self.hasNoElements )
   {
      return 0;
   }
   else if ( self.allElementsLoaded )
   {
      *out_is_overflow_ = YES;
      return 0;
   }
   else if ( [ self noNeedToLoadElementAtIndex: index_ ] )
   {
      return 0;
   }  
   else if ( self.isPagingDisabled )
   {
      return self.totalElementsCount - self.currentCount;
   }

   static const NSUInteger load_more_placeholder_size_ = 1;
   NSUInteger rest_of_the_items_ = self.totalElementsCount - self.currentCount;

   float items_count_for_index_path_ = 1 + index_;
   NSUInteger pages_expected_ = ceil( items_count_for_index_path_ / self.pageSize );
   NSUInteger elements_expected_ = pages_expected_ * self.pageSize;

   //check if paging disabled
   BOOL is_overflow_ = ( elements_expected_ >= self.totalElementsCount );
   if ( is_overflow_ )
   {
      *out_is_overflow_ = YES;
      return rest_of_the_items_ - load_more_placeholder_size_;
   }

   return elements_expected_ - self.currentCount;
}


#pragma mark -
#pragma mark TableViewHelper
-(NSUInteger)numberOfRows
{
   NSUInteger result_ = self.totalElementsCount;

   if ( self.hasNoElements )
   {
      // "Loading" cell or "No clips" cell.
      result_ = 1;
   }
   else if ( self.isPagingEnabled && !self.allElementsLoaded )
   {
      result_ =  self.currentCount + 1/*More shows cell*/;
   }
   
   return  result_;
}

-(NSInteger)currentCountToStartWith:( NSInteger )total_elements_count_
{
   if ( total_elements_count_ > 0 )
   {      
      if ( [ self isPagingDisabled ] )
      {
         self.currentCount = total_elements_count_;
      }
      
      NSUInteger current_count_ = MAX( self.pageSize, self.currentCount );
      current_count_ = MIN( current_count_, total_elements_count_ );      
      self.currentCount = current_count_;
   }

   return self.currentCount;
}


-(void)insertToTableView:( id<JUTableViewHolder> )table_view_holder_
             bottomCells:(NSUInteger)cells_count_
         overflowOccured:( BOOL )is_overflow_
{
   NSDebugLog( @"[BEGIN] : insertToBottomCells" ); 
   if ( 0 == cells_count_ )
   {
      NSDebugLog( @"NO cells to insert" );
      NSDebugLog( @"[END] : insertToBottomCells" );
      return;
   }

   NSArray* index_paths_ = [ self prepareIndexPathEntriesForBottomCells: cells_count_ ];

   NSDebugLog( @"index_path_[%d] : %@ .. %@", [ index_paths_ count ], [ index_paths_ objectAtIndex: 0 ], [ index_paths_ lastObject ] );
   NSDebugLog( @"page size : %d", [ self pageSize ] );

   [ table_view_holder_.tableView withinUpdates: ^void( void )
   {
       NSDebugLog( @"beginUpdates" );      
       NSArray* load_more_path_array_ = [ NSArray arrayWithObject: self.loadMoreIndexPath ];
       
       [ [ table_view_holder_ tableView ] reloadRowsAtIndexPaths: load_more_path_array_
                                            withRowAnimation: UITableViewRowAnimationNone ];
       
       
       [ [ table_view_holder_ tableView ] insertRowsAtIndexPaths: index_paths_ 
                                            withRowAnimation: UITableViewRowAnimationNone ];
       
       
       NSDebugLog( @"Updating currentCount..." );
       self.currentCount += cells_count_;
       if ( is_overflow_ && ( self.currentCount < self.totalElementsCount ) )
       {
          ++self.currentCount;
       }
       [ table_view_holder_ setCurrentCount: self.currentCount ];
       
       NSDebugLog( @"currentCount : %d", self.currentCount );
       NSDebugLog( @"endUpdates" );
    } ];
   
   NSDebugLog( @"[END] : insertToBottomCells" );
}

-(void)autoLoadingScrollTableView:( id<JUTableViewHolder> )table_view_holder_
                 toRowAtIndexPath:( NSIndexPath* )index_path_ 
                 atScrollPosition:( UITableViewScrollPosition )scroll_position_ 
                         animated:( BOOL )animated_
{
   NSDebugLog( @"[BEGIN] : autoLoadingScrollToRowAtIndexPath:[%d]", index_path_.row );
   NSDebugLog( @"  totalClipsCount == %d", self.totalElementsCount );
   NSDebugLog( @"  currentCount    == %d", self.currentCount );
   
   if ( [ self hasNoElements ] )
   {
      NSDebugLog( @"   No clips available" );
      NSDebugLog( @"[END] : autoLoadingScrollToRowAtIndexPath:[%d]", index_path_.row );      
      return;
   }
   
   BOOL is_overflow_ = NO;
   NSUInteger clips_to_add_ = [ self suggestElementsToAddCountForIndexPath: index_path_ 
                                                           overflowOccured: &is_overflow_];
   NSDebugLog( @"  clips_to_add_   == %d", clips_to_add_ );

   if ( 0 != clips_to_add_ )
   {
      NSDebugLog( @"   inserting cells" );   
      [ self insertToTableView: table_view_holder_
                   bottomCells: clips_to_add_ 
               overflowOccured: is_overflow_ ];
   }
   NSUInteger target_index_ = MIN( self.currentCount - clips_to_add_, index_path_.row );
   NSIndexPath* destination_ = [ NSIndexPath indexPathForRow: target_index_
                                                   inSection: index_path_.section ];
   
   NSDebugLog( @"   scrolling down to [%@]", destination_ );
   [ table_view_holder_.tableView scrollToRowAtIndexPath: destination_
                                        atScrollPosition: scroll_position_
                                                animated: animated_ ];
   
   NSDebugLog( @"[END] : autoLoadingScrollToRowAtIndexPath:[%d]", index_path_.row );   
}

#pragma mark -
#pragma mark Utils
+(NSArray*)defaultUpdateScopeForIndex:( NSUInteger )index_
{
   NSIndexPath* index_path_ = [ NSIndexPath indexPathForRow: index_
                                                  inSection: 0 ];
   NSArray* result_ = [ NSArray arrayWithObject: index_path_ ];
   
   return result_;
}

@end
