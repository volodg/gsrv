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

-(BOOL)isCurrentGameSateResponse
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
   return [ self count ] >= 6
        && [ self objectForKey: @"currentPlayer" ]
        && [ self objectForKey: @"field"         ]
        && [ self objectForKey: @"points"        ]
        && [ self objectForKey: @"state"         ]
        && [ self objectForKey: @"sym"           ]
        && [ self objectForKey: @"users"         ];
}

-(BOOL)isGetSymbolsResponse
{
   return [ self count ] == 2
       && [ self objectForKey: @"state" ]
       && [ self objectForKey: @"sym"   ];
}

-(BOOL)isCurrentGameSateResponse
{
   return [ self count ] >= 6
      && [ self objectForKey: @"currentPlayer" ]
      && [ self objectForKey: @"points"        ]
      && [ self objectForKey: @"state"         ]
      && [ self objectForKey: @"sym"           ]
      && [ self objectForKey: @"x"             ]
      && [ self objectForKey: @"y"             ];
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

-(BOOL)isCurrentGameSateResponse
{
   return [ self firstMatch: ^BOOL( id object_ )
   {
      return [ object_ isCurrentGameSateResponse ];
   } ] != nil;
}

@end
