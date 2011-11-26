#import "NSArray+Parser.h"

#import "MWSymb.h"
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
      symb_.symb  = first_object_;
      symb_.state = [ second_object_ integerValue ];
      [ symbols_ addObject: symb_ ];
   } ];
   return [ NSArray arrayWithArray: symbols_ ];
}

@end
