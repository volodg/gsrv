#import "MWGameStatistic+Parser.h"

@implementation MWGameStatistic (Parser)

+(id)gameStatisticWithDict:( NSDictionary* )dict_
{
    MWGameStatistic* result_ = [ [ [ self alloc ] init ] autorelease ];

    result_.count1placeBattle = [ [ dict_ objectForKey: @"count1placeBattle" ] integerValue ];
    result_.count2placeBattle = [ [ dict_ objectForKey: @"count2placeBattle" ] integerValue ];
    result_.count3placeBattle = [ [ dict_ objectForKey: @"count3placeBattle" ] integerValue ];
    result_.count4placeBattle = [ [ dict_ objectForKey: @"count4placeBattle" ] integerValue ];

    result_.count1placeDuel = [ [ dict_ objectForKey: @"count1placeDuel" ] integerValue ];
    result_.count2placeDuel = [ [ dict_ objectForKey: @"count2placeDuel" ] integerValue ];

    result_.experience  = [ [ dict_ objectForKey: @"experience"  ] integerValue ];
    result_.level       = [ [ dict_ objectForKey: @"level"       ] integerValue ];
    result_.totalStones = [ [ dict_ objectForKey: @"totalStones" ] integerValue ];
    result_.totalTime   = [ [ dict_ objectForKey: @"totalTime"   ] doubleValue ];
    result_.jadvise     = [ [ dict_ objectForKey: @"jadvise"     ] integerValue ];
    result_.usedmana    = [ [ dict_ objectForKey: @"usedmana"    ] integerValue ];

    result_.sp1 = [ [ dict_ objectForKey: @"sp1" ] integerValue ];
    result_.sp2 = [ [ dict_ objectForKey: @"sp2" ] integerValue ];
    result_.sp3 = [ [ dict_ objectForKey: @"sp3" ] integerValue ];
    result_.sp4 = [ [ dict_ objectForKey: @"sp4" ] integerValue ];

    result_.el1 = [ [ dict_ objectForKey: @"el1" ] integerValue ];
    result_.el2 = [ [ dict_ objectForKey: @"el2" ] integerValue ];
    result_.el3 = [ [ dict_ objectForKey: @"el3" ] integerValue ];
    result_.el4 = [ [ dict_ objectForKey: @"el4" ] integerValue ];

    result_.bst1 = [ [ dict_ objectForKey: @"bst1" ] integerValue ];
    result_.bst2 = [ [ dict_ objectForKey: @"bst2" ] integerValue ];
    result_.bst3 = [ [ dict_ objectForKey: @"bst3" ] integerValue ];
    result_.bst4 = [ [ dict_ objectForKey: @"bst4" ] integerValue ];

    return result_;
}

@end
