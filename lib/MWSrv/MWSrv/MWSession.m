#import "MWSession.h"

#import "MWApi.h"

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api = _api;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api release ];

   [ super dealloc ];
}

-(id)init
{
   self = [ super init ];

   if ( self )
   {
      self.api = [ [ MWApi new ] autorelease ];
   }

   return self;
}

+(id)currentSession
{
   static MWSession* instance_;
   if ( !instance_ )
   {
      instance_ = [ MWSession new ];
   }
   return instance_;
}

-(JFFAsyncOperation)authLoaderWithLogin:( NSString* )login_
{
   return nil;
}

@end
