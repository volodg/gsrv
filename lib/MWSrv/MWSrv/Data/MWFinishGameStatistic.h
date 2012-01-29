#import <Foundation/Foundation.h>

@interface MWFinishGameStatistic : NSObject

@property ( nonatomic, assign ) NSUInteger level;
@property ( nonatomic, assign ) NSUInteger manaused;
@property ( nonatomic, retain ) NSString* playerNick;
@property ( nonatomic, assign ) NSUInteger points;
@property ( nonatomic, assign ) NSUInteger stonesused;
@property ( nonatomic, assign ) NSTimeInterval tperturn;

@end
