#import "NSString+Parser.h"

@implementation NSString (Parser)

-(NSArray*)arrayOfStringsSeparatedByComma
{
   NSArray* result_ = [ self componentsSeparatedByString: @"," ];
   return [ result_ select: ^BOOL( id chunk_ )
   {
      return [ chunk_ length ] > 0;
   } ];
}

@end
