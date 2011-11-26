#import <Foundation/Foundation.h>

@interface NSObject (Parser)

-(BOOL)isStatusOKResponse;
-(BOOL)isStartGameStateResponse;
-(BOOL)isGameStartedResponse;

@end
