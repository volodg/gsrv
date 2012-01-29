#import "NSObject+Parser.h"

@implementation NSObject (MWSrvParser)

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

-(BOOL)isGameFinishedResponse
{
    return NO;
}

-(BOOL)isStatisticResponse
{
    return NO;
}

-(BOOL)isKeepAlive
{
    return NO;
}

@end

@implementation NSDictionary ( Parser )

-(BOOL)isGameStartedResponse
{
   return [ [ self objectForKey: @"reqId" ] isEqualToString: @"3" ];
}

-(BOOL)isGameFinishedResponse
{
    return [ [ self objectForKey: @"reqId" ] isEqualToString: @"2" ];
}

-(BOOL)isGetSymbolsResponse
{
   return [ [ self objectForKey: @"reqId" ] isEqualToString: @"4" ];
}

-(BOOL)isCurrentGameSateResponse
{
   return [ [ self objectForKey: @"reqId" ] isEqualToString: @"1" ];
}

-(BOOL)isStatisticResponse
{
    return [ self count ] >= 6
    && [ self objectForKey: @"experience"  ]
    && [ self objectForKey: @"level"       ]
    && [ self objectForKey: @"totalStones" ]
    && [ self objectForKey: @"jadvise"     ]
    && [ self objectForKey: @"usedmana"    ]
    && [ self objectForKey: @"sp1"         ];
}

-(BOOL)isKeepAlive
{
    return [ [ self objectForKey: @"reqId" ] isEqualToString: @"7" ];
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
