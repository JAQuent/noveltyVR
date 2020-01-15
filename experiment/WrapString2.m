function wrappedString=WrapString2(string, maxLineLength, lineSpacing)
% wrappedString=WrapString2(string,[maxLineLength], [lineSpacing])
% 
% Wraps text by changing spaces into linebreaks (e.g. '\n'), making each line as
% long as possible without exceeding maxLineLength (default 74
% characters). The number of linebreaks can be controlled by lineSpacing 
% (default 1). WrapString2 does not break words, even if you have a word
% that exceeds maxLineLength. The returned "wrappedString" is identical to
% the supplied "string" except for the conversion of some spaces into
% linebreaks. Besides making the text look pretty, wrapping the text will
% make the printout narrow enough that it can be sent by email and
% received as sent, not made hard to read by mindless breaking of every
% line.
% 
% Note that this schemes is based on counting characters, not pixels, so
% it will give a fairly even right margin only for monospaced fonts, not
% proportionally spaced fonts. The more general solution would be based on
% counting pixels, not characters, using either Screen 'TextWidth' or
% TextBounds.

% This function is a modification of the WrapString function
% (http://psychtoolbox.org/docs/WrapString) from Psychtoolbox. 

% 06/30/02 dgp Wrote it.
% 10/02/02 dgp Make it clear that maxLineLength is in characters, not pixels.
% 09/20/09 mk Improve argument handling as per suggestion of Peter April.
% 01/14/20 Alex Quent added possibility to add more than one new line by
%          using the lineSpacing argument. 

if nargin > 3 || nargout>1 
	error('Usage: wrappedString=WrapString(string, [maxLineLength], [lineSpacing])\n');
end

if nargin<2
    maxLineLength=[];
    lineSpacing = 1;
elseif nargin < 3
    lineSpacing = 1;
end

if isempty(maxLineLength) || isnan(maxLineLength)
	maxLineLength=74;
end


newLineStr = repmat('\n', [1, lineSpacing]);
eol=sprintf(newLineStr);
wrapped='';
while length(string)>maxLineLength
	l=min([strfind(char(string),eol) length(string)+1]);
	if l<maxLineLength
		% line is already short enough
		[wrapped,string]=onewrap(wrapped,string,l, newLineStr);
	else
		s=strfind(char(string),' ');
		n=find(s<maxLineLength);
		if ~isempty(n)
			% ignore spaces before the furthest one before maxLineLength
			s=s(max(n):end);
		end
		% break at nearest space, linebreak, or end.
		s=sort([s l]);
		[wrapped,string]=onewrap(wrapped,string,s(1), newLineStr);
	end
end
wrappedString=[wrapped string];
return

function [wrapped,string]=onewrap(wrapped,string,n, newLineStr)
if n>length(string)
	wrapped=[wrapped string];
	string=[];
	return
end
wrapped=[wrapped string(1:n-1) sprintf(newLineStr)];
string=string(n+1:end);
return