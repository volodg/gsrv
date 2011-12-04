#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

@class MWCurrentGameState;

@interface MWSession : NSObject

@property ( nonatomic, retain, readonly ) MWCurrentGameState* currentGameState;

+(id)sessionWithLogin:( NSString* )login_;

-(JFFAsyncOperation)playBattleground;
-(JFFAsyncOperation)getSymbolsCount:( NSUInteger )count_;
-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_
                                      scores:( NSUInteger )points_;

-(JFFAsyncOperation)waitStep;
-(JFFAsyncOperation)skipStep;

-(void)exitGame;

@end
