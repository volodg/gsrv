#import <Foundation/Foundation.h>

@interface NSObject (MWSrvParser)

-(BOOL)isGameStartedResponse;
-(BOOL)isGetSymbolsResponse;
-(BOOL)isCurrentGameSateResponse;
-(BOOL)isGameFinishedResponse;
-(BOOL)isStatisticResponse;
-(BOOL)isKeepAlive;

@end
