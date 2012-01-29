#import <Foundation/Foundation.h>

@class MWDoStepState;
@class MWStartGameState;
@class MWCurrentGameState;

@interface MWSessionGameState : NSObject

@property ( nonatomic, retain ) MWStartGameState* startGameState;
@property ( nonatomic, retain ) MWDoStepState* lastStepState;
@property ( nonatomic, assign ) BOOL active;//GTODO remove
@property ( nonatomic, retain ) NSMutableDictionary* statisticByCid;
@property ( nonatomic, retain ) MWCurrentGameState* currentGameState;

@end
