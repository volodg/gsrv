#import "NSArray+Parser.h"

#import "MWSymb.h"
#import "MWSymbWithCoords.h"

#import "NSString+Parser.h"

@implementation NSArray (Parser)

+(id)arraySymbolsWithDictionary:( NSDictionary* )dict_
{
   NSArray* letters_ = [ [ dict_ objectForKey: @"sym"   ] arrayOfStringsSeparatedByComma ];
   NSArray* states_  = [ [ dict_ objectForKey: @"state" ] arrayOfStringsSeparatedByComma ];

   NSMutableArray* symbols_ = [ NSMutableArray arrayWithCapacity: [ states_ count ] ];
   [ letters_ transformWithArray: states_
                       withBlock: ^void( id first_object_, id second_object_ )
   {
      MWSymb* symb_ = [ [ MWSymb new ] autorelease ];
      symb_.symb  = [ first_object_ uppercaseString ];
      symb_.state = [ second_object_ integerValue ];
      [ symbols_ addObject: symb_ ];
   } ];
   return [ NSArray arrayWithArray: symbols_ ];
}

+(id)arraySymbolsAndCoordsWithDictionary:( NSDictionary* )dict_
{
   NSArray* letters_ = [ [ dict_ objectForKey: @"sym"   ] arrayOfStringsSeparatedByComma ];
   NSArray* states_  = [ [ dict_ objectForKey: @"state" ] arrayOfStringsSeparatedByComma ];
   NSArray* x_crds_  = [ [ dict_ objectForKey: @"x"     ] arrayOfStringsSeparatedByComma ];
   NSArray* y_crds_  = [ [ dict_ objectForKey: @"y"     ] arrayOfStringsSeparatedByComma ];

   NSUInteger size_ = [ letters_ count ];

   NSMutableArray* result_ = [ NSMutableArray arrayWithCapacity: size_ ];

   for ( NSUInteger index_ = 0; index_ < size_; ++index_ )
   {
      MWSymb* symb_ = [ [ MWSymb new ] autorelease ];
      symb_.symb  = [ [ letters_ objectAtIndex: index_ ] uppercaseString ];
      symb_.state = [ [ states_ objectAtIndex: index_ ] integerValue ];

      MWSymbWithCoords* smartSymb_ = [ [ MWSymbWithCoords new ] autorelease ];
      smartSymb_.x = [ [ x_crds_ objectAtIndex: index_ ] integerValue ];
      smartSymb_.y = [ [ y_crds_ objectAtIndex: index_ ] integerValue ];
      smartSymb_.symb = symb_;

      [ result_ addObject: smartSymb_ ];
   }

   return result_;
}

@end
