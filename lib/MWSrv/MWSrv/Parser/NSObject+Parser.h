#import <Foundation/Foundation.h>

@interface NSObject (Parser)

-(BOOL)isStatusOKResponse;
-(BOOL)isGameStartedResponse;
-(BOOL)isGetSymbolsResponse;
-(BOOL)isCurrentGameSateResponse;

@end
