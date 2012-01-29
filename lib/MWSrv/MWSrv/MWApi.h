#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>

#import <Foundation/Foundation.h>

@class MWDoStepState;

@interface MWApi : NSObject

-(JFFAsyncOperation)authWithLogin:( NSString* )login_
                              sid:( NSString* )sid_;

-(JFFAsyncOperation)playBattlegroundForSid:( NSString* )sid_;
-(JFFAsyncOperation)playDuelForSid:( NSString* )sid_;

-(JFFAsyncOperation)getSrvStateWithSid:( NSString* )sid_;

-(JFFAsyncOperation)exitGameWithSid:( NSString* )sid_;

-(JFFAsyncOperation)returnSymbolsWithSid:( NSString* )sid_
                                 symbols:( NSArray* )symbols_;

-(JFFAsyncOperation)getSymbolsWithSid:( NSString* )sid_
                                count:( NSUInteger )count_;

-(JFFAsyncOperation)doStepWithSid:( NSString* )sid_
                   symbsAndCoords:( NSArray* )step_
                            state:( MWDoStepState* )state_;

-(JFFAsyncOperation)keepAliveForSid:( NSString* )sid_;

-(JFFAsyncOperation)statisticForSid:( NSString* )sid_;

@end
