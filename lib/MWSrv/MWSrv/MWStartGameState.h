#import <Foundation/Foundation.h>

@interface MWStartGameState : NSObject

@property ( nonatomic, assign ) NSUInteger field;
@property ( nonatomic, retain ) NSArray* users;
@property ( nonatomic, retain ) NSArray* symbols;

+(id)startGameStateWithDictionary:( NSDictionary* )dict_;

@end
