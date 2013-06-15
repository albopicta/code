1;

function errorMatrix = fun(k)

  trials = 2000;

  % I don't have randsrc
  mapUniformToX =  @(x) 2*(ceil(x-0.5) - 0.5);
%  mapUniformToX = @(x) 2*(ceil(x-6/7) - 0.5);
  X = mapUniformToX(rand(4, trials));

  N = randn(4, trials);
  H = [1,4,1,3;1,1,2,2;1,0,0.01,0;1,0,1,1];
%  H = [1,0,0,0;0,0.7,0,0;0,0,0.25,0;0,0,0,0.1];
%  H = [1,0,0,0;0,0.5,0,0;1,0,0,0;0,0,0,1];

  i=1;
  while (i <= trials)
    Y(:,i) = H * X(:,i) + k * N(:,i);
    ++i;
  end

  CY = H * transpose(H) + k *eye(4);
%  XLMMSE = transpose(H) * inv(CY) * Y;
  XLMMSE = inv(H) * Y % sub optimal approximation

  signThatDoesNotResultInZero = @(x) sign(sign(x) - 0.1);
  Xdeciphered = signThatDoesNotResultInZero(XLMMSE);

  errorMatrix = 1/2 * abs(Xdeciphered - X); % 1 if error, 0 otherwise
endfunction

function num = errors(errorMatrix)
  len = length(errorMatrix);
  summ = 0;
  for i=1:len
    summ = summ + sign(norm(errorMatrix(:,i)));
  end
  num = summ / len;
endfunction

H = [1,4,1,3;1,1,2,2;1,0,0.01,0;1,0,1,1];
%H = [1,0,0,0;0,0.7,0,0;0,0,0.25,0;0,0,0,0.1];
%H = [1,0,0,0;0,0.5,0,0;1,0,0,0;0,0,0,1];
CY = H * transpose(H) + 1 *eye(4);
trials = 2000;

sigmaOneMistakeMatrix = fun(1);
mistakeCov = 1/trials * sigmaOneMistakeMatrix * transpose(sigmaOneMistakeMatrix);

analyticalMistakeCov = eye(4) - transpose(H) * inv(CY) * H;
analyticalSuboptimalMistakeCov = inv(H) * 1 * transpose(inv(H));

%differenceMatrix = analyticalMistakeCov - mistakeCov;

samples=100;

SNR = linspace(0.001,40,samples);
sigmaSquared = 10.^(-0.1 .* SNR);

j=1;
while (j <= samples)
  errorsMade(j) = errors(fun(sigmaSquared(j)));
  ++j;
end

figure(1);
semilogy(SNR, errorsMade);
title('probability of error as a function of SNR');
xlabel('SNR');
ylabel('probability of error (log)');
grid;
