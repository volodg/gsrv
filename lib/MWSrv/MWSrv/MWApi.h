#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>

#import <Foundation/Foundation.h>

@interface MWApi : NSObject

-(JFFAsyncOperation)authLoaderWithLogin:( NSString* )login_;

@end
