#import "MWGameStatistic.h"

@implementation MWGameStatistic

@synthesize cid = _cid;
@synthesize count1placeBattle = _count1placeBattle;
@synthesize count2placeBattle = _count2placeBattle;
@synthesize count3placeBattle = _count3placeBattle;
@synthesize count4placeBattle;
@synthesize count1placeDuel;
@synthesize count2placeDuel;
@synthesize experience;
@synthesize level;
@synthesize totalStones;
@synthesize totalTime;
@synthesize jadvise;
@synthesize usedmana;
@synthesize sp1 = _sp1;
@synthesize sp2 = _sp2;
@synthesize sp3 = _sp3;
@synthesize sp4 = _sp4;
@synthesize el1;
@synthesize el2;
@synthesize el3;
@synthesize el4;
@synthesize bst1;
@synthesize bst2;
@synthesize bst3;
@synthesize bst4;

-(void)dealloc
{
    [ _cid release ];

    [ super dealloc ];
}

-(NSString*)description
{
    return [ NSString stringWithFormat: @"<MWGameStatistic cid: %@ sp1: %d sp2: %d sp3: %d sp4: %d >"
            , _cid
            , _sp1
            , _sp2
            , _sp3
            , _sp4 ];
}

@end
