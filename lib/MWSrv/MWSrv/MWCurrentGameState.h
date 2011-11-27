#import <Foundation/Foundation.h>

@interface MWCurrentGameState : NSObject

@property ( nonatomic, retain ) NSArray* symbolsAndCoods;

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_;

@end
