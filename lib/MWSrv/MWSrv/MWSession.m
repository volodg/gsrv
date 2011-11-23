#import "MWSession.h"

#import "MWApi.h"

static MWSession* instance_;

@interface MWSession ()

@property ( nonatomic, retain ) NSDate* lastLoginDate;
@property ( nonatomic, retain ) MWApi* api;
@property ( nonatomic, retain ) NSString* login;
@property ( nonatomic, retain ) NSString* sid;

@end

@implementation MWSession

@synthesize lastLoginDate = _lastLoginDate;
@synthesize api           = _api;
@synthesize login         = _login;
@synthesize sid           = _sid;

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

-(BOOL)loginDateExpared
{
   NSTimeInterval interval_ = [ self.lastLoginDate timeIntervalSinceNow ];
   return interval_ < -4.8 * 60;
}

-(JFFAsyncOperation)authLoader
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      JFFAsyncOperation loader_ = [ self.api authWithLogin: self.login ];

      JFFDidFinishAsyncOperationHandler did_finish_operation_ = ^( id result_, NSError* error_ )
      {
         self.lastLoginDate = [ NSDate date ];
      };
      loader_ = [ self asyncOperationForPropertyWithName: @"sid"
                                          asyncOperation: loader_
                                  didFinishLoadDataBlock: did_finish_operation_ ];
      if ( self.lastLoginDate == nil ||
          [ self loginDateExpared ] )
      {
         self.sid = nil;
      }
      return loader_( progress_callback_, cancel_callback_, done_callback_ );
   } copy ] autorelease ];
}

-(JFFAsyncOperation)privateGetListOfGames
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api getListOfGamesForSid: self.sid ]( progress_callback_
                                                         , cancel_callback_
                                                         , done_callback_ );
   };

   return sequenceOfAsyncOperations( auth_loader_, cmd_loader_, nil );
}

-(JFFAsyncOperation)getSrvState
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api getSrvStateWithSid: self.sid ]( progress_callback_
                                                       , cancel_callback_
                                                       , done_callback_ );
   };

   return sequenceOfAsyncOperations( auth_loader_, cmd_loader_, nil );
}

-(JFFAsyncOperation)getListOfGames
{
   //GTODO put in load balancer
   return [ self privateGetListOfGames ];
}

-(JFFAsyncOperation)createGameWithName:( NSString* )name_
{
   JFFAsyncOperation auth_loader_ = [ self authLoader ];
   JFFAsyncOperation cmd_loader_ = ^( JFFAsyncOperationProgressHandler progress_callback_
                                     , JFFCancelAsyncOperationHandler cancel_callback_
                                     , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      return [ self.api createGameWithName: name_ sid: self.sid ]( progress_callback_
                                                                  , cancel_callback_
                                                                  , done_callback_ );
   };

   //GTODO put in load balancer
   return sequenceOfAsyncOperations( auth_loader_
                                    , cmd_loader_
                                    , [ self privateGetListOfGames ]
                                    , [ self getSrvState ]
                                    , nil );
}

@end
