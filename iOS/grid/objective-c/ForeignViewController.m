/*
 *   grid is a game written in Haskell
 *   Copyright (C) 2018 karamellpelle@hotmail.com
 *
 *   This file is part of grid.
 *
 *   grid is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   grid is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with grid.  If not, see <http://www.gnu.org/licenses/>.
*/
#import "ForeignViewController.h"
#import "IOSForeign.h"
#import <QuartzCore/QuartzCore.h>
#import "IOSPlayers.h"
#import "IOSScreen.h"
#import "IOSViewController.h"



// global pointer to the canonical ForeignViewController
UIViewController* theForeignViewController = nil;

static ForeignDelegate* foreign_delegate = nil;
static UINavigationController* nav_vc = nil;
static UIViewController* dummy_vc = nil;
static ForeignBrowserViewController* browser_vc = nil;


// create 'theForeignViewController'
void foreign_make()
{
    // create delegate
    foreign_delegate = [[ForeignDelegate alloc] init];
    
    // UINavigationController with dummy root ViewController
    dummy_vc = [[UIViewController alloc] init];
    nav_vc = [[UINavigationController alloc] initWithRootViewController:dummy_vc];
    [nav_vc setDelegate:foreign_delegate];

    // push actual ViewController
    browser_vc = [[ForeignBrowserViewController alloc] init];
    [nav_vc pushViewController:browser_vc animated:NO];
    
    theForeignViewController = nav_vc;
}


// destroy 'theForeignViewController'
void foreign_destroy()
{
    [theForeignViewController release];
}



// note: if this fails, it might be inferred by ios_loadTexPreMult.
void foreign_screenshot(GLenum target, GLuint tex, uint wth, uint hth)
{
    glBindTexture( target, tex );
    
    GLubyte* image_data = (GLubyte*) malloc( wth * hth * 4 * sizeof(GLubyte) );
    
    CGColorSpaceRef color_space = CGColorSpaceCreateDeviceRGB();
    
    // all textures are represented by premultiplied alpha.
    // for discussion of premultiplied alpha, see
    // * http://home.comcast.net/~tom_forsyth/blog.wiki.html#[[Premultiplied%20alpha]]
    // * http://keithp.com/~keithp/porterduff/p253-porter.pdf
    CGContextRef context = CGBitmapContextCreate( image_data, wth, hth, 
                                                  8, 4 * wth, color_space,  
                                                  kCGImageAlphaPremultipliedLast 
                                                  //kCGImageAlphaLast is not implemented!
                                                );

    // since tex has typilally lesser size, we want anti-aliasing
    //CGContextSetAllowsAntialiasing( context, true );
    
    // added 27 feb 2013 instead of above...
    CGContextSetAllowsAntialiasing( context, true );
    CGContextSetShouldAntialias( context, true );
    CGContextSetBlendMode( context, kCGBlendModeCopy );
    
    float wth0 = wth;
    float hth0 = hth;
    CGSize size = [theForeignViewController.view bounds].size;
    float wth1 = size.width;
    float hth1 = size.height;
    
    float scale = MIN( wth0 / wth1, hth0 / hth1 );
    float wth2 = scale * wth1;
    float hth2 = scale * hth1;
    float tx = 0.5 * (wth0 - wth2);
    float ty = 0.5 * (hth0 - hth2);
    float x = wth2 / wth1;
    float y = hth2 / hth1;
    CGContextConcatCTM( context, CGAffineTransformMake(x, 0, 0, y, tx, ty) );
    
    //CGContextConcatCTM( context, CGAffineTransformMake(1, 0, 0, -1, 0, hth) );

    // render into image_data
    [theForeignViewController.view.layer renderInContext:context];
    
    glTexImage2D( target, 0, GL_RGBA, wth, hth, 0, GL_RGBA, GL_UNSIGNED_BYTE, image_data );
    
    CGContextRelease( context );
    
    CGColorSpaceRelease( color_space );
    free( image_data );

}

// escape from Foreign
void foreign_escape()
{
    // fixme: release resources
    
    // set platform state to Haskell
    ios_foreignEndForeign();

    
}

@implementation ForeignBrowserViewController

@synthesize alert = alert_;


-(id) init
{
    self = [super initWithNibName:nil bundle:nil];

    return self;
}


-(id) initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    NSAssert( NO, @"ForeignViewController: should use 'init' instead" );
    return [self init];
}


-(void) viewDidAppear:(BOOL)animated
{
    [alert_ show];
}



-(BOOL) shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation
{
    return ios_shouldAutorotateToInterfaceOrientation( toInterfaceOrientation );
}


- (void) loadView
{
    printf("ForeignViewController::loadView\n");
    
    // create UIView for this ViewController
    CGRect frame = [[theIOSViewController view] frame]; //[[UIScreen mainScreen] applicationFrame];
    UIView* contentView = [[UIView alloc] initWithFrame:frame];
    contentView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
    contentView.backgroundColor = [UIColor whiteColor];
    self.view = contentView;
    
    // if playing as a local player, open GameCenter
    GKLocalPlayer* local_player = [GKLocalPlayer localPlayer];
    if (local_player.isAuthenticated)
    {
        printf("fixme: create GKGameCenterViewController");
    }
    else
    {

        // make alert
/*
        alert_ = [[UIAlertView alloc] initWithTitle:@"GameCenter"
                                            message:@"no player"
                                           delegate:self
                                  cancelButtonTitle:@"back"
                                  otherButtonTitles:nil];
*/
/*
        // make imageview
        NSString* path = [[NSBundle mainBundle] pathForResource:@"data/tmp/texture.png"
                                                ofType:nil];
        UIImage* image = [[UIImage alloc] initWithContentsOfFile:path];
        UIImageView* imageview = [[UIImageView alloc] initWithImage:image];
        [self.view addSubview:imageview];
        [imageview release];
        [image release];
*/
        UIWebView* webview = [[UIWebView alloc] initWithFrame: frame];
        webview.autoresizingMask =  UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
        
        [webview loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://www.haskell.org"]]];
        [self.view addSubview:webview];
        [webview release];
        
    }
}




- (void)alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex
{    
    if ( buttonIndex == 0 )
    {
        foreign_escape();
    }
}


- (void)alertViewCancel:(UIAlertView *)alertView
{
    NSLog(@"alert view was cancelled!");
    
}


- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    
}


@end


@implementation ForeignDelegate


-(void) navigationController:(UINavigationController *)navigationController didShowViewController:(UIViewController *)viewController animated:(BOOL)animated
{

}

-(void) navigationController:(UINavigationController *)navigationController willShowViewController:(UIViewController *)viewController animated:(BOOL)animated
{
    if ( viewController == dummy_vc )
    {
        [nav_vc pushViewController:browser_vc animated:NO];
        foreign_escape();

    }
}


@end
