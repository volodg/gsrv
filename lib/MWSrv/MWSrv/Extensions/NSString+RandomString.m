#import "NSString+RandomString.h"

static NSString* const letters = @"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

@implementation NSString (RandomString)

+(NSString*)randomStringWithLength:( NSUInteger )length_
{
    NSMutableString* randomString = [ NSMutableString stringWithCapacity: length_ ];

    for ( int i = 0; i < length_; i++ )
    {
        [ randomString appendFormat: @"%C", [ letters characterAtIndex: rand() % [ letters length ] ] ];
    }

    return randomString;
}

@end
