#import "JFFAsyncOperationContinuity.h"

#import "JFFCancelAyncOperationBlockHolder.h"
#import "JFFAsyncOperationsPredefinedBlocks.h"
#import "JFFDidFinishAsyncOperationHookHolder.h"

#import <JFFScheduler/JFFScheduler.h>

#import <Foundation/Foundation.h>

#include <assert.h>

typedef JFFAsyncOperation (*MergeTwoLoadersPtr)( JFFAsyncOperation, JFFAsyncOperation );

static JFFAsyncOperation createEmptyLoaderBlock()
{
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      done_callback_( [ NSNull null ], nil );
      return JFFEmptyCancelAsyncOperationBlock;
   } copy ] autorelease ];
}

static JFFAsyncOperation MergeLoaders( MergeTwoLoadersPtr merger_, NSArray* blocks_ )
{
   if ( ![ blocks_ lastObject ] )
      return createEmptyLoaderBlock();

   JFFAsyncOperation first_block_ = [ blocks_ objectAtIndex: 0 ];

   for ( JFFAsyncOperation second_block_ in blocks_ )
   {
      if ( second_block_ == first_block_ )
         continue;

      first_block_ = merger_( first_block_, second_block_ );
   }

   return first_block_;
}

static JFFAsyncOperation sequenceOfAsyncOperationsPair( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_ )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   first_loader_ = [ [ first_loader_ copy ] autorelease ];
   second_loader_ = [ [ second_loader_ copy ] autorelease ];

   if ( second_loader_ == nil )
      return first_loader_;

   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFCancelAyncOperationBlockHolder* block_holder_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      JFFCancelAsyncOperation first_cancel_ = first_loader_( progress_callback_
                                                           , cancel_callback_
                                                           , ^void( id result_, NSError* error_ )
      {
         if ( error_ )
         {
            if ( done_callback_ )
               done_callback_( nil, error_ );
         }
         else
         {
            block_holder_.cancelBlock = second_loader_( progress_callback_
                                                       , cancel_callback_
                                                       , done_callback_ );
         }
      } );
      if ( !block_holder_.cancelBlock )
         block_holder_.cancelBlock = first_cancel_;

      return block_holder_.onceCancelBlock;
   } copy ] autorelease ];
}

JFFAsyncOperation sequenceOfAsyncOperations( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_, ... )
{
   JFFAsyncOperation first_block_ = first_loader_;

   va_list args;
   va_start( args, second_loader_ );
   for ( JFFAsyncOperation second_block_ = second_loader_; second_block_ != nil; second_block_ = va_arg( args, JFFAsyncOperation ) )
   {
      first_block_ = sequenceOfAsyncOperationsPair( first_block_, second_block_ );
   }
   va_end( args );

   return first_block_;
}

JFFAsyncOperation sequenceOfAsyncOperationsArray( NSArray* loaders_ )
{
   return MergeLoaders( sequenceOfAsyncOperationsPair, loaders_ );
}

static JFFAsyncOperation trySequenceOfAsyncOperationsPair( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_ )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   first_loader_ = [ [ first_loader_ copy ] autorelease ];
   second_loader_ = [ [ second_loader_ copy ] autorelease ];

   if ( second_loader_ == nil )
      return first_loader_;

   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFCancelAyncOperationBlockHolder* block_holder_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      done_callback_ = [ [ done_callback_ copy ] autorelease ];

      JFFCancelAsyncOperation first_cancel_ = first_loader_( progress_callback_, cancel_callback_, ^void( id result_, NSError* error_ )
      {
         if ( error_ )
         {
            block_holder_.cancelBlock = second_loader_( progress_callback_, cancel_callback_, done_callback_ );
         }
         else
         {
            if ( done_callback_ )
               done_callback_( result_, nil );
         }
      } );
      if ( !block_holder_.cancelBlock )
         block_holder_.cancelBlock = first_cancel_;

      return block_holder_.onceCancelBlock;
   } copy ] autorelease ];
}

JFFAsyncOperation trySequenceOfAsyncOperations( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_, ... )
{
   JFFAsyncOperation first_block_ = first_loader_;

   va_list args;
   va_start( args, second_loader_ );
   for ( JFFAsyncOperation second_block_ = second_loader_; second_block_ != nil; second_block_ = va_arg( args, JFFAsyncOperation ) )
   {
      first_block_ = trySequenceOfAsyncOperationsPair( first_block_, second_block_ );
   }
   va_end( args );

   return first_block_;
}

JFFAsyncOperation trySequenceOfAsyncOperationsArray( NSArray* loaders_ )
{
   return MergeLoaders( trySequenceOfAsyncOperationsPair, loaders_ );
}

static JFFAsyncOperation groupOfAsyncOperationsPair( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_ )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   first_loader_ = [ [ first_loader_ copy ] autorelease ];
   second_loader_ = [ [ second_loader_ copy ] autorelease ];

   if ( second_loader_ == nil )
      return first_loader_;

   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      __block BOOL loaded_ = NO;
      JFFResultContext* error_holder_ = [ [ JFFResultContext new ] autorelease ];

      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      JFFDidFinishAsyncOperationHandler result_block_ = ^void( id result_, NSError* error_ )
      {
         if ( loaded_ )
         {
            error_ = error_ ? error_ : error_holder_.error;
            if ( done_callback_ )
               done_callback_( error_ ? nil : [ NSNull null ], error_ );
            return;
         }
         loaded_ = YES;
         error_holder_.error = error_;
      };

      __block BOOL block_canceled_ = NO;

      JFFCancelAyncOperationBlockHolder* cancel_holder1_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];
      JFFCancelAyncOperationBlockHolder* cancel_holder2_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
      JFFCancelAsyncOperationHandler cancel_callback1_ = ^void( BOOL canceled_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder2_.onceCancelBlock( canceled_ );
            if ( cancel_callback_ )
               cancel_callback_( canceled_ );
         }
      };

      JFFCancelAsyncOperationHandler cancel_callback2_ = ^void( BOOL canceled_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder1_.onceCancelBlock( canceled_ );
            if ( cancel_callback_ )
               cancel_callback_( canceled_ );
         }
      };

      result_block_ = [ [ result_block_ copy ] autorelease ];

      JFFDidFinishAsyncOperationHandler done_callback1_ = ^void( id result_, NSError* error_ )
      {
         cancel_holder1_.cancelBlock = nil;
         result_block_( result_, error_ );
      };
      JFFDidFinishAsyncOperationHandler done_callback2_ = ^void( id result_, NSError* error_ )
      {
         cancel_holder2_.cancelBlock = nil;
         result_block_( result_, error_ );
      };

      cancel_holder1_.cancelBlock = first_loader_( progress_callback_, cancel_callback1_, done_callback1_ );
      cancel_holder2_.cancelBlock = second_loader_( progress_callback_, cancel_callback2_, done_callback2_ );
       
      return [ [ ^void( BOOL cancel_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder1_.onceCancelBlock( cancel_ );
            cancel_holder2_.onceCancelBlock( cancel_ );
            if ( cancel_callback_ )
               cancel_callback_( cancel_ );
         }
      } copy ] autorelease ];
   } copy ] autorelease ];
}

JFFAsyncOperation groupOfAsyncOperations( JFFAsyncOperation first_loader_, JFFAsyncOperation second_loader_, ... )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   JFFAsyncOperation first_block_ = first_loader_;

   va_list args;
   va_start( args, second_loader_ );
   for ( JFFAsyncOperation second_block_ = second_loader_; second_block_ != nil; second_block_ = va_arg( args, JFFAsyncOperation ) )
   {
      first_block_ = groupOfAsyncOperationsPair( first_block_, second_block_ );
   }
   va_end( args );

   return first_block_;
}

JFFAsyncOperation groupOfAsyncOperationsArray( NSArray* blocks_ )
{
   return MergeLoaders( groupOfAsyncOperationsPair, blocks_ );
}

static JFFDidFinishAsyncOperationHandler cancelSafeResultBlock( JFFDidFinishAsyncOperationHandler result_block_
                                                               , JFFCancelAyncOperationBlockHolder* cancel_holder_ )
{
   result_block_ = [ [ result_block_ copy ] autorelease ];
   return [ [ ^void( id result_, NSError* error_ )
   {
      cancel_holder_.cancelBlock = nil;
      result_block_( result_, error_ );
   } copy ] autorelease ];
}

static JFFAsyncOperation failOnFirstErrorGroupOfAsyncOperationsPair( JFFAsyncOperation first_loader_
                                                                    , JFFAsyncOperation second_loader_ )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   first_loader_ = [ [ first_loader_ copy ] autorelease ];
   second_loader_ = [ [ second_loader_ copy ] autorelease ];

   if ( second_loader_ == nil )
      return first_loader_;

   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      __block BOOL loaded_ = NO;
      __block BOOL done_ = NO;

      JFFCancelAyncOperationBlockHolder* cancel_holder1_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];
      JFFCancelAyncOperationBlockHolder* cancel_holder2_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      JFFDidFinishAsyncOperationHandler result_block_ = ^void( id result_, NSError* error_ )
      {
         BOOL first_error_ = error_ && !done_;
         if ( loaded_ || first_error_ )
         {
            if ( first_error_ )
            {
               cancel_holder1_.onceCancelBlock( YES );
               cancel_holder2_.onceCancelBlock( YES );
            }

            done_ = YES;
            if ( done_callback_ )
               done_callback_( error_ ? nil : [ NSNull null ], error_ );
            return;
         }
         loaded_ = YES;
      };

      __block BOOL block_canceled_ = NO;

      cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
      JFFCancelAsyncOperationHandler cancel_callback1_ = [ [ ^void( BOOL canceled_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder2_.onceCancelBlock( canceled_ );
            if ( cancel_callback_ )
               cancel_callback_( canceled_ );
         }
      } copy ] autorelease ];

      JFFCancelAsyncOperationHandler cancel_callback2_ = [ [ ^void( BOOL canceled_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder1_.onceCancelBlock( canceled_ );
            if ( cancel_callback_ )
               cancel_callback_( canceled_ );
         }
      } copy ] autorelease ];

      JFFCancelAsyncOperation cancel1_ = first_loader_( progress_callback_
                                                       , cancel_callback1_
                                                       , cancelSafeResultBlock( result_block_, cancel_holder1_ ) );

      cancel_holder1_.cancelBlock = done_ ? JFFEmptyCancelAsyncOperationBlock : cancel1_;

      JFFCancelAsyncOperation cancel2_ = done_
         ? JFFEmptyCancelAsyncOperationBlock
         : second_loader_( progress_callback_
                          , cancel_callback2_
                          , cancelSafeResultBlock( result_block_, cancel_holder2_ ) );

      cancel_holder2_.cancelBlock = done_ ? JFFEmptyCancelAsyncOperationBlock : cancel2_;

      return [ [ ^void( BOOL cancel_ )
      {
         if ( !block_canceled_ )
         {
            block_canceled_ = YES;
            cancel_holder1_.onceCancelBlock( cancel_ );
            cancel_holder2_.onceCancelBlock( cancel_ );
            if ( cancel_callback_ )
               cancel_callback_( cancel_ );
         }
      } copy ] autorelease ];
   } copy ] autorelease ];
}

JFFAsyncOperation failOnFirstErrorGroupOfAsyncOperations( JFFAsyncOperation first_loader_
                                                         , JFFAsyncOperation second_loader_
                                                         , ... )
{
   if ( first_loader_ == nil )
      return createEmptyLoaderBlock();

   JFFAsyncOperation first_block_ = first_loader_;

   va_list args;
   va_start( args, second_loader_ );
   for ( JFFAsyncOperation second_block_ = second_loader_; second_block_ != nil; second_block_ = va_arg( args, JFFAsyncOperation ) )
   {
      first_block_ = failOnFirstErrorGroupOfAsyncOperationsPair( first_block_, second_block_ );
   }
   va_end( args );

   return first_block_;
}

JFFAsyncOperation failOnFirstErrorGroupOfAsyncOperationsArray( NSArray* blocks_ )
{
   return MergeLoaders( failOnFirstErrorGroupOfAsyncOperationsPair, blocks_ );
}

JFFAsyncOperation asyncOperationWithFinishCallbackBlock( JFFAsyncOperation loader_
                                                        , JFFDidFinishAsyncOperationHandler finish_callback_block_ )
{
   finish_callback_block_ = [ [ finish_callback_block_ copy ] autorelease ];
   loader_ = [ [ loader_ copy ] autorelease ];
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      return loader_( progress_callback_, cancel_callback_, ^void( id result_, NSError* error_ )
      {
         if ( finish_callback_block_ )
            finish_callback_block_( result_, error_ );
         if ( done_callback_ )
            done_callback_( result_, error_ );
      } );
   } copy ] autorelease ];
}

JFFAsyncOperation asyncOperationWithFinishHookBlock( JFFAsyncOperation loader_
                                                    , JFFDidFinishAsyncOperationHook finishCallbackHook_ )
{
   assert( finishCallbackHook_ );// should not be nil"
   finishCallbackHook_ = [ [ finishCallbackHook_ copy ] autorelease ];
   loader_ = [ [ loader_ copy ] autorelease ];
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      return loader_( progress_callback_, cancel_callback_, ^void( id result_, NSError* error_ )
      {
         finishCallbackHook_( result_, error_, done_callback_ );
      } );
   } copy ] autorelease ];
}

JFFAsyncOperation asyncOperationWithDoneBlock( JFFAsyncOperation loader_
                                              , JFFSimpleBlock done_callback_hook_ )
{
   loader_ = [ [ loader_ copy ] autorelease ];
   if ( nil == done_callback_hook_ )
      return loader_;

   done_callback_hook_ = [ [ done_callback_hook_ copy ] autorelease ];
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
      cancel_callback_ = [ [ ^void( BOOL canceled_ )
      {
         done_callback_hook_();

         if ( cancel_callback_ )
            cancel_callback_( canceled_ );
      } copy ] autorelease ];

      done_callback_ = [ [ done_callback_ copy ] autorelease ];
      done_callback_ = ^void( id result_, NSError* error_ )
      {
         done_callback_hook_();

         if ( done_callback_ )
            done_callback_( result_, error_ );
      };
      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

JFFAsyncOperation asyncOperationWithResult( id result_ )
{
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      if ( done_callback_ )
         done_callback_( result_, nil );
      return JFFEmptyCancelAsyncOperationBlock;
   } copy ] autorelease ];
}

JFFAsyncOperation repeatAsyncOperation( JFFAsyncOperation native_loader_
                                       , PredicateBlock predicate_
                                       , NSTimeInterval delay_
                                       , NSInteger max_repeat_count_ )
{
   assert( native_loader_ );// can not be nil
   assert( predicate_     );// can not be nil

   native_loader_ = [ [ native_loader_ copy ] autorelease ];
   predicate_     = [ [ predicate_     copy ] autorelease ];

   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      progress_callback_ = [ [ progress_callback_ copy ] autorelease ];
      cancel_callback_   = [ [ cancel_callback_   copy ] autorelease ];
      done_callback_     = [ [ done_callback_     copy ] autorelease ];

      JFFCancelAyncOperationBlockHolder* holder_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      JFFDidFinishAsyncOperationHookHolder* hoolHolder_ = [ [ JFFDidFinishAsyncOperationHookHolder new ] autorelease ];

      __block NSInteger currentLeftCount = max_repeat_count_;

      JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^( id result_
                                                               , NSError* error_
                                                               , JFFDidFinishAsyncOperationHandler done_callback_ )
      {
         JFFResultContext* context_ = [ [ JFFResultContext new ] autorelease ];
         context_.result = result_;
         context_.error  = error_ ;
         if ( !predicate_( context_ ) || currentLeftCount == 0 )
         {
            hoolHolder_.finishHookBlock = nil;
            if ( done_callback_ )
               done_callback_( result_, error_ );
         }
         else
         {
            currentLeftCount = currentLeftCount > 0
               ? currentLeftCount - 1
               : currentLeftCount;

            JFFAsyncOperation loader_ = asyncOperationWithFinishHookBlock( native_loader_
                                                                          , hoolHolder_.finishHookBlock );
            loader_ = asyncOperationAfterDelay( delay_, loader_ );

            holder_.cancelBlock = loader_( progress_callback_, cancel_callback_, done_callback_ );
         }
      };

      hoolHolder_.finishHookBlock = finish_callback_hook_;

      JFFAsyncOperation loader_ = asyncOperationWithFinishHookBlock( native_loader_
                                                                    , finish_callback_hook_ );

      holder_.cancelBlock = loader_( progress_callback_
                                    , cancel_callback_
                                    , done_callback_ );

      return [ [ ^( BOOL canceled_ )
      {
         hoolHolder_.finishHookBlock = nil;
         holder_.onceCancelBlock( canceled_ );
      } copy ] autorelease ];
   } copy ] autorelease ];
}

JFFAsyncOperation asyncOperationAfterDelay( NSTimeInterval delay_
                                           , JFFAsyncOperation loader_ )
{
   loader_ = [ [ loader_ copy ] autorelease ];
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      progress_callback_ = [ [ progress_callback_ copy ] autorelease ];
      cancel_callback_   = [ [ cancel_callback_   copy ] autorelease ];
      done_callback_     = [ [ done_callback_     copy ] autorelease ];

      JFFCancelAyncOperationBlockHolder* lc_holder_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];

      __block JFFScheduler* scheduler_ = [ JFFScheduler new ];

      __block BOOL unsubscribed_ = NO;

      JFFCancelScheduledBlock sch_cancel_ = [ scheduler_ addBlock: ^( JFFCancelScheduledBlock sch_cancel_ )
      {
         [ scheduler_ release ];
         scheduler_ = nil;
         sch_cancel_();

         lc_holder_.cancelBlock = unsubscribed_
            ? loader_( nil, nil, nil )
            : loader_( progress_callback_, cancel_callback_, done_callback_ );
      } duration: delay_ ];

      lc_holder_.cancelBlock = ^( BOOL canceled_ )
      {
         if ( canceled_ )
         {
            [ scheduler_ release ];
            scheduler_ = nil;
            sch_cancel_();
         }
         else
         {
            unsubscribed_ = YES;
         }
         if ( cancel_callback_ )
            cancel_callback_( canceled_ );
      };

      return lc_holder_.onceCancelBlock;
   } copy ] autorelease ];
}
