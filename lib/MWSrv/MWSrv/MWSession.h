#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

typedef void (^MWServerCommandsHandler)( NSArray* commands_ );

@interface MWSession : NSObject

@property ( nonatomic, copy ) MWServerCommandsHandler handler;

+(id)sessionWithLogin:( NSString* )login_;

-(JFFAsyncOperation)playBattleground;
-(JFFAsyncOperation)getSymbolsCount:( NSUInteger )count_;
-(JFFAsyncOperation)doStepWithSymbsAndCoords:( NSArray* )step_;

-(void)exitGame;

@end
