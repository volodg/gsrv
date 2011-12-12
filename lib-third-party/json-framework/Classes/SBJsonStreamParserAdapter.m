/*
 Copyright (c) 2010, Stig Brautaset.
 All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are
 met:
 
   Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
  
   Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
 
   Neither the name of the the author nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#import "SBJsonStreamParserAdapter.h"

@interface SBJsonStreamParserAdapter ()

@property ( nonatomic, retain ) NSMutableArray *array;
@property ( nonatomic, retain ) NSMutableDictionary *dict;
@property ( nonatomic, retain ) NSMutableArray *stack;
@property ( nonatomic, retain ) NSMutableArray *keyStack;

- (void)pop;
- (void)parser:(SBJsonStreamParser*)parser found:(id)obj;

@end



@implementation SBJsonStreamParserAdapter

@synthesize delegate;
@synthesize levelsToSkip;
@synthesize array = _array;
@synthesize dict = _dict;
@synthesize stack = _stack;
@synthesize keyStack = _keyStack;

#pragma mark Housekeeping

-(void)dealloc
{
   [ _array    release ];
	[ _dict     release ];
	[ _keyStack release ];
	[ _stack    release ];

   [ super dealloc ];
}

- (id)init {
	self = [super init];
	if (self) {
		self.keyStack = [[[NSMutableArray alloc] initWithCapacity:32]autorelease ];
		self.stack = [[[NSMutableArray alloc] initWithCapacity:32]autorelease];

		currentType = SBJsonStreamParserAdapterNone;
	}
	return self;
}	


#pragma mark Private methods

- (void)pop {
	[self.stack removeLastObject];
	self.array = nil;
	self.dict = nil;
	currentType = SBJsonStreamParserAdapterNone;
	
	id value = [self.stack lastObject];
	
	if ([value isKindOfClass:[NSArray class]]) {
		self.array = value;
		currentType = SBJsonStreamParserAdapterArray;
	} else if ([value isKindOfClass:[NSDictionary class]]) {
		self.dict = value;
		currentType = SBJsonStreamParserAdapterObject;
	}
}

- (void)parser:(SBJsonStreamParser*)parser found:(id)obj {
	NSParameterAssert(obj);
	
	switch (currentType) {
		case SBJsonStreamParserAdapterArray:
			[self.array addObject:obj];
			break;

		case SBJsonStreamParserAdapterObject:
			NSParameterAssert(self.keyStack.count);
			[self.dict setObject:obj forKey:[self.keyStack lastObject]];
			[self.keyStack removeLastObject];
			break;
			
		case SBJsonStreamParserAdapterNone:
			if ([obj isKindOfClass:[NSArray class]]) {
				[delegate parser:parser foundArray:obj];
			} else {
				[delegate parser:parser foundObject:obj];
			}				
			break;

		default:
			break;
	}
}


#pragma mark Delegate methods

- (void)parserFoundObjectStart:(SBJsonStreamParser*)parser {
	if (++depth > self.levelsToSkip) {
		self.dict = [[NSMutableDictionary new]autorelease];
		[self.stack addObject:self.dict];
		currentType = SBJsonStreamParserAdapterObject;
	}
}

- (void)parser:(SBJsonStreamParser*)parser foundObjectKey:(NSString*)key_ {
	[self.keyStack addObject:key_];
}

- (void)parserFoundObjectEnd:(SBJsonStreamParser*)parser {
	if (depth-- > self.levelsToSkip) {
		id value = self.dict;
		[self pop];
		[self parser:parser found:value];
	}
}

- (void)parserFoundArrayStart:(SBJsonStreamParser*)parser {
	if (++depth > self.levelsToSkip) {
		self.array = [[NSMutableArray new]autorelease];
		[self.stack addObject:self.array];
		currentType = SBJsonStreamParserAdapterArray;
	}
}

- (void)parserFoundArrayEnd:(SBJsonStreamParser*)parser {
	if (depth-- > self.levelsToSkip) {
		id value = self.array;
		[self pop];
		[self parser:parser found:value];
	}
}

- (void)parser:(SBJsonStreamParser*)parser foundBoolean:(BOOL)x {
	[self parser:parser found:[NSNumber numberWithBool:x]];
}

- (void)parserFoundNull:(SBJsonStreamParser*)parser {
	[self parser:parser found:[NSNull null]];
}

- (void)parser:(SBJsonStreamParser*)parser foundNumber:(NSNumber*)num {
	[self parser:parser found:num];
}

- (void)parser:(SBJsonStreamParser*)parser foundString:(NSString*)string {
	[self parser:parser found:string];
}

@end
