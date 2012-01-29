#import "MWSessionGameState.h"

@implementation MWSessionGameState

@synthesize startGameState   = _startGameStatel;
@synthesize lastStepState    = _lastStepState;
@synthesize active           = _active;
@synthesize statisticByCid   = _statisticByCid;
@synthesize currentGameState = _currentGameState;

-(void)dealloc
{
    [ _startGameStatel  release ];
    [ _lastStepState    release ];
    [ _statisticByCid   release ];
    [ _currentGameState release ];

    [ super dealloc ];
}

-(NSDictionary*)statisticByCid
{
    if ( !_statisticByCid )
    {
        _statisticByCid = [ NSMutableDictionary new ];
    }
    return _statisticByCid;
}

@end
