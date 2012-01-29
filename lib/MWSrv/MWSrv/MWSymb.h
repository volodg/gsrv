#import <Foundation/Foundation.h>

@interface MWSymb : NSObject

@property ( nonatomic, retain ) NSString* symb;
@property ( nonatomic, assign ) NSUInteger state;

+(id)symbWithSymb:( NSString* )symb_ state:( NSUInteger )state_;

@end
