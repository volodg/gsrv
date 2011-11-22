#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

@interface MWSession : NSObject

+(id)currentSession;
+(id)sessionWithLogin:( NSString* )login_;

-(JFFAsyncOperation)createGameWithName:( NSString* )name_;

-(JFFAsyncOperation)getListOfGames;

@end
