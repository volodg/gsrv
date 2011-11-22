#import <JFFAsyncOperations/JFFAsyncOperationsBlockDefinitions.h>

#import <Foundation/Foundation.h>

@interface MWApi : NSObject

-(JFFAsyncOperation)authWithLogin:( NSString* )login_;

-(JFFAsyncOperation)createGameWithName:( NSString* )name_
                                   sid:( NSString* )sid_;

-(JFFAsyncOperation)getListOfGamesForSid:( NSString* )sid_;

-(JFFAsyncOperation)getSrvStateWithSid:( NSString* )sid_;

@end
