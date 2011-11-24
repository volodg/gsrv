#import "MWApi.h"

#import "NSString+RandomString.h"

#import <AMFUnarchiver.h>

static NSUInteger sidLength_ = 32;
static NSString* const host_format_ = @"http://test.bwf.org.ua:3333/%@";

@interface NSURL (MWApi)

+(id)URLWithSid:( NSString* )sid_;

@end

@implementation NSURL (MWApi)

+(id)URLWithSid:( NSString* )sid_
{
   NSString* str_ = [ NSString stringWithFormat: host_format_, sid_ ];
   return [ NSURL URLWithString: str_ ];
}

@end

@interface MWApi ()

@property ( nonatomic, retain, readonly ) NSDictionary* headers;

@end

@implementation MWApi

@synthesize headers = _headers;

-(void)dealloc
{
   [ _headers release ];

   [ super dealloc ];
}

-(NSDictionary*)headers
{
   if ( !_headers )
   {
      _headers = [ [ NSDictionary alloc ] initWithObjectsAndKeys:
                  @"application/json", @"Content-type"
                  , nil ];
   }
   return _headers;
}

-(JFFAsyncOperation)authWithLogin:( NSString* )login_
{
   NSString* sid_ = [ NSString randomStringWithLength: sidLength_ ];
   NSURL*    url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_format_ = @"{\"cmd\":\"enter\",\"env\":1,\"cid\":\"%@\"}";
   NSString* post_        = [ NSString stringWithFormat: post_format_, login_ ? login_ : @"" ];
   NSData*   post_data_   = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , self.headers );

   JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                , NSError* error_
                                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      id< JNUrlResponse > response_ = result_;
      if ( response_ )
      {
         if ( 200 == response_.statusCode )
         {
            done_callback_( sid_, nil );
         }
         else
         {
            NSString* error_format_ = @"Invalid auth response code: %d";
            NSString* error_description_ = [ NSString stringWithFormat:
                                            error_format_
                                            , response_.statusCode ];
            done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
         }
      }
      else
      {
         done_callback_( nil, error_ );
      }
   };

   return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(JFFAsyncOperation)playBattlegroundForSid:( NSString* )sid_
{
   NSURL* url_ = [ NSURL URLWithSid: sid_ ];

   NSString* post_      = @"{\"cmd\":\"playBattleground\"}";
   NSData*   post_data_ = [ post_ dataUsingEncoding: NSUTF8StringEncoding ];

   JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                        , post_data_
                                                        , self.headers );

   JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                , NSError* error_
                                                                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      id< JNUrlResponse > response_ = result_;
      if ( response_ )
      {
         if ( 200 == response_.statusCode )
         {
            done_callback_( [ NSNull null ], nil );
         }
         else
         {
            NSString* error_format_ = @"Invalid playBattleground response code: %d";
            NSString* error_description_ = [ NSString stringWithFormat:
                                            error_format_
                                            , response_.statusCode ];
            done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
         }
      }
      else
      {
         done_callback_( nil, error_ );
      }
   };

   return asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
}

-(NSArray*)splitResponseData:( NSData* )data_
{
   NSMutableArray* result_ = [ NSMutableArray new ];
   for ( NSUInteger index_ = 0; index_ < data_.length; )
   {
      NSData* size_data_ = [ data_ subdataWithRange: NSMakeRange( index_, sizeof( int ) ) ];
      int size_ = CFSwapInt32BigToHost(*(int*)([size_data_ bytes]));

      {
         NSRange chunkRange_ = NSMakeRange( index_ + sizeof( int ), size_ );
         NSData* chunk_ = [ data_ subdataWithRange: chunkRange_ ];
         [ result_ addObject: chunk_ ];
      }

      index_ += size_ + sizeof( int );
   }
   return [ result_ autorelease ];
}

-(JFFAsyncOperation)getSrvStateWithSid:( NSString* )sid_
{
   return [ [ ^( JFFAsyncOperationProgressHandler progress_callback_
                , JFFCancelAsyncOperationHandler cancel_callback_
                , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      //NSURL* url_ = [ NSURL URLWithString: @"http://test.bwf.org.ua:3333" ];
      NSURL* url_ = [ NSURL URLWithSid: sid_ ];

      static char data_[1] = { 0 };
      NSData* post_data_ = [ NSData dataWithBytes: data_ length: 1 ];

      NSMutableData* response_data_ = [ NSMutableData data ];

      JFFAsyncOperation loader_ = chunkedURLResponseLoader( url_
                                                           , post_data_
                                                           , nil );

      JFFDidFinishAsyncOperationHook finish_callback_hook_ = ^void( id result_
                                                                   , NSError* error_
                                                                   , JFFDidFinishAsyncOperationHandler done_callback_ )
      {
         id< JNUrlResponse > response_ = result_;
         if ( response_ )
         {
            if ( 200 == response_.statusCode )
            {
               NSData* data_ = response_data_;
               NSArray* chunks_ = [ self splitResponseData: data_ ];

               id new_result_ = [ chunks_ map: ^id( id chunk_ )
               {
                  return [ AMFUnarchiver unarchiveObjectWithData: chunk_
                                                        encoding: kAMF3Encoding ];
               } ];

               done_callback_( new_result_, nil );
            }
            else
            {
               NSString* error_format_ = @"Invalid getSrvState response code: %d";
               NSString* error_description_ = [ NSString stringWithFormat:
                                               error_format_
                                               , response_.statusCode ];
               done_callback_( nil, [ JFFError errorWithDescription: error_description_ ] );
            }
         }
         else
         {
            done_callback_( nil, error_ );
         }
      };

      progress_callback_ = [ [ progress_callback_ copy ] autorelease ];
      progress_callback_ = ^( id progress_data_ )
      {
         [ response_data_ appendData: progress_data_ ];
         if ( progress_callback_ )
            progress_callback_( progress_data_ );
      };

      loader_ = asyncOperationWithFinishHookBlock( loader_, finish_callback_hook_ );
      return loader_( progress_callback_
                     , cancel_callback_
                     , done_callback_  );
   } copy ] autorelease ];
}

@end
