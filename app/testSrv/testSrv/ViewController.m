#import "ViewController.h"

#import "MWPlayer.h"


@interface ViewController ()

@property ( nonatomic, retain ) MWPlayer* player1;
@property ( nonatomic, retain ) MWPlayer* player2;
@property ( nonatomic, retain ) MWPlayer* player3;
@property ( nonatomic, retain ) MWPlayer* player4;

@end

@implementation ViewController

@synthesize firstLetter;
@synthesize secondLetter;
@synthesize player1 = _player1;
@synthesize player2 = _player2;
@synthesize player3 = _player3;
@synthesize player4 = _player4;

-(void)dealloc
{
    [ firstLetter  release ];
    [ secondLetter release ];
    [ _player1     release ];
    [ _player2     release ];
    [ _player3     release ];
    [ _player4     release ];

    [ super dealloc ];
}

-(void)loginAndPlayPlayer2
{
   self.player2 = [ MWPlayer playerWithLogin: @"user2" ];
   [ self.player2 start ];
}

-(void)loginAndPlayPlayer3
{
    self.player2 = [ MWPlayer playerWithLogin: @"user3" ];
    [ self.player2 start ];
}

-(void)loginAndPlayPlayer4
{
    self.player2 = [ MWPlayer playerWithLogin: @"user4" ];
    [ self.player2 start ];
}


-(void)test
{
   self.player1 = [ MWPlayer playerWithLogin: @"user1" ];
   [ self.player1 start ];

   [ self performSelector: @selector( loginAndPlayPlayer2 )
               withObject: nil
               afterDelay: 12.0 ];

    [ self performSelector: @selector( loginAndPlayPlayer3 )
                withObject: nil
                afterDelay: 8.0 ];

    [ self performSelector: @selector( loginAndPlayPlayer4 )
                withObject: nil
                afterDelay: 17.0 ];
}

-(IBAction)doStep1:(id)sender {
   [ self.player1 doStepWithSymb: self.firstLetter.text ];
}

-(IBAction)doStep2:(id)sender {
   [ self.player2 doStepWithSymb: self.secondLetter.text ];
}

- (IBAction)doSkip2:(id)sender {
   [ self.player2 skipStep ];
}

- (IBAction)doSkip1:(id)sender {
   [ self.player1 skipStep ];
}

- (IBAction)doWait1:(id)sender {
   [ self.player1 wait ];
}

- (IBAction)doWait2:(id)sender {
   [ self.player2 wait ];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

- (void)viewDidLoad
{
   [ super viewDidLoad ];
	// Do any additional setup after loading the view, typically from a nib.

   [ self test ];
}

- (void)viewDidUnload
{
   [self setFirstLetter:nil];
   [self setSecondLetter:nil];
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
}

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
}

- (void)viewWillDisappear:(BOOL)animated
{
	[super viewWillDisappear:animated];
}

- (void)viewDidDisappear:(BOOL)animated
{
	[super viewDidDisappear:animated];
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
   return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
}

@end
