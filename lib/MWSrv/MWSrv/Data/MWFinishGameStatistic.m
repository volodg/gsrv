#import "MWFinishGameStatistic.h"

@implementation MWFinishGameStatistic

@synthesize level      = _level;
@synthesize manaused   = _manaused;
@synthesize playerNick = _playerNick;
@synthesize points     = _points;
@synthesize stonesused = _stonesused;
@synthesize tperturn   = _tperturn;

-(void)dealloc
{
    [ _playerNick release ];

    [ super dealloc ];
}

@end
