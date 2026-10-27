function [newspi]=spi(n,index);
    n=squeeze(n);
    newspi=empdis(n,index);
    newspi=single(round(norminv(newspi),3));  %yrs,masking,pentads

function y=empdis(d,years)

%  Compute empirical probability

n=length(d);bp=zeros(n,1);

%[a,b]=sort(d(years));
%bp(b)=1:length(years);

for i=1:n
    bp(i,1)=sum(d(years,1)<=d(i,1));
end
n=length(years);
bp(bp<=0)=1;
% Gringorten plotting position

y=(bp-0.33)./(n+0.33);

%y=(bp-0.44)./(n+0.12);
%  Can be replaced with the Weibull plotting position
% y=(bp)./(n+1);
%bp



