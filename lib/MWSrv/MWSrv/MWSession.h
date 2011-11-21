#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>//GTODO remove

#import <Foundation/Foundation.h>

@interface MWSession : NSObject

+(id)currentSession;

-(JFFAsyncOperation)authLoaderWithLogin:( NSString* )login_;

@end
