#import <Foundation/Foundation.h>

@interface MWCurrentGameState : NSObject

@property ( nonatomic, retain ) NSArray* symbolsAndCoods;
@property ( nonatomic, retain ) NSArray* allPoints;
@property ( nonatomic, assign ) NSUInteger ownPoints;
@property ( nonatomic, assign ) NSUInteger opponentPoints;

+(id)currentGameStateWithDictionary:( NSDictionary* )dict_
                            userCid:( NSString* )cid_
                          usersCids:( NSArray* )cids_;

@end
