#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@property (retain, nonatomic) IBOutlet UITextField *firstLetter;
@property (retain, nonatomic) IBOutlet UITextField *secondLetter;

- (IBAction)doStep1:(id)sender;
- (IBAction)doStep2:(id)sender;

- (IBAction)doSkip1:(id)sender;
- (IBAction)doSkip2:(id)sender;

@end
