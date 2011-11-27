#import <Foundation/Foundation.h>

@interface MWPlayer : NSObject

+(id)playerWithLogin:( NSString* )login_;

-(void)start;
-(void)doStepWithSymb:( NSString* )symb_;
-(void)wait;
-(void)skipStep;

@end
