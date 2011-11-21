#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

@interface MWSession : NSObject

+(id)currentSession;
+(id)sessionWithLogin:( NSString* )login_;

-(JFFAsyncOperation)authLoader;

@end
