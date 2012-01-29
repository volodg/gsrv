#import "MWFinishGameStatistic+Parser.h"

@implementation MWFinishGameStatistic (Parser)

+(id)finishGameStatisticWithDict:( NSDictionary* )dict_
{
    MWFinishGameStatistic* result_ = [ [ [ self alloc ] init ] autorelease ];

    result_.level      = [ [ dict_ objectForKey: @"level"      ] integerValue ];
    result_.manaused   = [ [ dict_ objectForKey: @"manaused"   ] integerValue ];
    result_.playerNick = [   dict_ objectForKey: @"player"     ];
    result_.points     = [ [ dict_ objectForKey: @"points"     ] integerValue ];
    result_.stonesused = [ [ dict_ objectForKey: @"stonesused" ] integerValue ];
    result_.tperturn   = [ [ dict_ objectForKey: @"tperturn"   ] doubleValue ];

    return result_;
}

@end
