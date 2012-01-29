#import <Foundation/Foundation.h>

@interface MWCurrentGameState : NSObject

@property ( nonatomic, retain ) NSArray* symbolsAndCoods;
@property ( nonatomic, retain ) NSArray* allPoints;

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_;

@end
