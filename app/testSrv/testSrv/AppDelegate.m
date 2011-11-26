#import "AppDelegate.h"

#import "ViewController.h"

#import "MWPlayer.h"

@interface AppDelegate ()

@property ( nonatomic, retain ) MWPlayer* player1;
@property ( nonatomic, retain ) MWPlayer* player2;

@end

@implementation AppDelegate

@synthesize window         = _window;
@synthesize viewController = _viewController;
@synthesize player1        = _player1;
@synthesize player2        = _player2;

- (void)dealloc
{
   [ _window release ];
   [ _viewController release ];
   [ _player1 release ];
   [ _player2 release ];

   [ super dealloc ];
}

-(void)loginAndPlayPlayer2
{
   self.player2 = [ MWPlayer playerWithLogin: @"testUser2" ];
   [ self.player2 start ];
}

-(void)test
{
   self.player1 = [ MWPlayer playerWithLogin: @"testUser1" ];
   [ self.player1 start ];

   [ self performSelector: @selector( loginAndPlayPlayer2 )
               withObject: nil
               afterDelay: 2.0 ];
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    // Override point for customization after application launch.
   self.viewController = [[[ViewController alloc] initWithNibName:@"ViewController" bundle:nil] autorelease];
   self.window.rootViewController = self.viewController;
   [ self.window makeKeyAndVisible ];

   [ self test ];

   return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
   /*
    Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
    */
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
   /*
    Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    */
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
   /*
    Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
    */
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
   /*
    Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    */
}

- (void)applicationWillTerminate:(UIApplication *)application
{
   /*
    Called when the application is about to terminate.
    Save data if appropriate.
    See also applicationDidEnterBackground:.
    */
}

@end
