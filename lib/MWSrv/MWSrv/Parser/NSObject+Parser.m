#import "NSObject+Parser.h"

@implementation NSObject (Parser)

-(BOOL)isStatusOKResponse
{
   return NO;
}

-(BOOL)isGameStartedResponse
{
   return NO;
}

-(BOOL)isGetSymbolsResponse
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

-(BOOL)isGameStartedResponse
{
   return [ self count ] == 4
       && [ self objectForKey: @"field" ]
       && [ self objectForKey: @"field" ]
       && [ self objectForKey: @"sym"   ]
       && [ self objectForKey: @"users" ];
}

-(BOOL)isGetSymbolsResponse
{
   return [ self count ] == 2
       && [ self objectForKey: @"state" ]
       && [ self objectForKey: @"sym"   ];
}

@end

@implementation NSArray ( Parser )

-(BOOL)isGameStartedResponse
{
   return [ self firstMatch: ^BOOL( id object_ )
   {
      return [ object_ isGameStartedResponse ];
   } ] != nil;
}

-(BOOL)isGetSymbolsResponse
{
   return [ self firstMatch: ^BOOL( id object_ )
   {
      return [ object_ isGetSymbolsResponse ];
   } ] != nil;
}

@end
