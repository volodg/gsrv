#import "NSObject+Parser.h"

@implementation NSObject (Parser)

-(BOOL)isStatusOKResponse
{
   return NO;
}

-(BOOL)isStartGameStateResponse
{
   return NO;
}

-(BOOL)isGameStartedResponse
{
   return NO;
}

@end

@implementation NSDictionary ( Parser )

-(BOOL)isStatusOKResponse
{
   return [ self count ] == 1
     && [ [ self objectForKey: @"status" ] integerValue ] == 1;
}

-(BOOL)isStartGameStateResponse
{
   return [ self count ] >= 4
       && [ self objectForKey: @"field" ]
       && [ self objectForKey: @"field" ]
       && [ self objectForKey: @"sym" ]
       && [ self objectForKey: @"users" ];
}

@end

@implementation NSArray ( Parser )

-(BOOL)isGameStartedResponse
{
   return [ self firstMatch: ^BOOL( id object_ )
   {
      return [ object_ isStartGameStateResponse ];
   } ] != nil;
}

@end
