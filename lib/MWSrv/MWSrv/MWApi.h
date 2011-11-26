#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>

#import <Foundation/Foundation.h>

@interface MWApi : NSObject

-(JFFAsyncOperation)authWithLogin:( NSString* )login_
                              sid:( NSString* )sid_;

-(JFFAsyncOperation)playBattlegroundForSid:( NSString* )sid_;

-(JFFAsyncOperation)getSrvStateWithSid:( NSString* )sid_;

-(JFFAsyncOperation)exitGameWithSid:( NSString* )sid_;

-(JFFAsyncOperation)getSymbolsWithSid:( NSString* )sid_
                                count:( NSUInteger )count_;

-(JFFAsyncOperation)doStepWithSid:( NSString* )sid_
                   symbsAndCoords:( NSArray* )step_;

@end
