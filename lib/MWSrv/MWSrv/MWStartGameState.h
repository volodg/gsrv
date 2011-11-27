#import <Foundation/Foundation.h>

@interface MWStartGameState : NSObject

@property ( nonatomic, assign ) NSUInteger field;
@property ( nonatomic, retain ) NSArray* users;
@property ( nonatomic, retain ) NSArray* symbols;
@property ( nonatomic, retain ) NSString* currentPlayer;
@property ( nonatomic, assign ) BOOL youFirst;

+(id)startGameStateWithDictionary:( NSDictionary* )dict_;

@end
