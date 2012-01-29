#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

@class MWDoStepState;
@class MWSessionGameState;
@class MWCurrentGameState;

typedef enum {
    MWGameTypeDuel,
    MWGameTypeBattleground
} MWGameType;

@interface MWSession : NSObject

@property ( nonatomic, assign, readonly ) MWGameType gameType;

@property ( nonatomic, retain, readonly ) MWSessionGameState* sessionState;
@property ( nonatomic, retain, readonly ) NSArray* allPoints;
@property ( nonatomic, retain, readonly ) NSArray* startPoints;
@property ( nonatomic, retain, readonly ) NSArray* userNames;

@property ( nonatomic, retain, readonly ) NSArray* finishGameStatistic;
@property ( nonatomic, copy ) JFFDidFinishAsyncOperationHandler finishGameCallback;

+(id)sessionWithLogin:( NSString* )login_;

-(JFFAsyncOperation)playBattleground;
-(JFFAsyncOperation)playDuel;

-(JFFAsyncOperation)getSymbolsCount:( NSUInteger )count_
                      returnSymbols:( NSArray* )symbols_;

-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_
                                       state:( MWDoStepState* )state_;

-(JFFAsyncOperation)waitStep;
-(JFFAsyncOperation)skipStep;

-(void)exitGame;

//-(JFFAsyncOperation)statisticLoader;

-(JFFAsyncOperation)mergeLoader1:( JFFAsyncOperation )loader1_
                         loader2:( JFFAsyncOperation )loader2_
                         loader3:( JFFAsyncOperation )loader3_;

@end
