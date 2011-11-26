#import <Foundation/Foundation.h>

@class MWSymb;

@interface MWSymbWithCoords : NSObject

@property ( nonatomic, retain ) MWSymb* symb;
@property ( nonatomic, assign ) NSUInteger x;
@property ( nonatomic, assign ) NSUInteger y;

@end
