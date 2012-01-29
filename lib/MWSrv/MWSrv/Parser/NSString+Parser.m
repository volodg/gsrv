#import "NSString+Parser.h"

@implementation NSString (MWSrvParser)

-(NSArray*)arrayOfStringsSeparatedByComma
{
   NSArray* result_ = [ self componentsSeparatedByString: @"," ];
   return [ result_ select: ^BOOL( id chunk_ )
   {
      return [ chunk_ length ] > 0;
   } ];
}

@end
