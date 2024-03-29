#import "UIWebView+HideWhenLoading.h"

static char property_key_;

@class JUIWebViewDelegateProxy;

@interface UIWebView (HideWhenLoadingInternal)

@property ( nonatomic, retain ) JUIWebViewDelegateProxy* proxy;

@end

@interface JUIWebViewDelegateProxy : NSObject < UIWebViewDelegate >

@property ( nonatomic, retain ) UIWebView* webView;

@end

@implementation JUIWebViewDelegateProxy

@synthesize webView = _webView;

-(void)dealloc
{
   [ _webView release ];

   [ super dealloc ];
}

#pragma mark UIWebViewDelegate

-(void)didFinishLoading
{
   self.webView.hidden = NO;

   self.webView.delegate = nil;
   self.webView.proxy = nil;
}

-(void)webViewDidFinishLoad:( UIWebView* )web_view_
{
   [ self didFinishLoading ];
}

-(void)webView:( UIWebView* )web_view_ didFailLoadWithError:( NSError* )error_
{
   [ self didFinishLoading ];
}

@end

@implementation UIWebView (HideWhenLoading)

-(JUIWebViewDelegateProxy*)proxy
{
   return ( JUIWebViewDelegateProxy* )objc_getAssociatedObject( self, &property_key_ );
}

-(void)setProxy:( JUIWebViewDelegateProxy* )proxy_
{
   objc_setAssociatedObject( self, &property_key_, proxy_, OBJC_ASSOCIATION_RETAIN_NONATOMIC ) ;   
}

-(void)hideWhenLoadingHTMLString:( NSString* )html_string_
{
   if ( !self.proxy )
   {
      self.proxy = [ [ JUIWebViewDelegateProxy new ] autorelease ];
      self.proxy.webView = self;
   }
   self.delegate = self.proxy;
   self.hidden = YES;
   [ self loadHTMLString: html_string_ baseURL: nil ];
}

@end
