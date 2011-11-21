#import "MWSession.h"

#import "MWApi.h"

static MWSession* instance_;

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api           = _api;
@synthesize login         = _login;

-(void)dealloc
{
   [ _lastLoginDate release ];
   [ _api release ];
   [ _login release ];

   [ super dealloc ];
}

-(id)initWithLogin:( NSString* )login_
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
   return instance_;
}

+(id)sessionWithLogin:( NSString* )login_
{
   [ instance_ release ];
   instance_ = [ [ self alloc ] initWithLogin: login_ ];
   return instance_;
}

-(JFFAsyncOperation)authLoader
{
   return [ self.api authLoaderWithLogin: self.login ];
}

@end
