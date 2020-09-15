
const
  CAppVersion = '0.0.1';
  CAppName = 'Substitute';
  CBuild = 'build ' + {$I %DATE%} + ' ' + {$I %TIME%};
  CCompiler = 'Free Pascal ' + {$I %FPCVERSION%} + ' ' + {$I %FPCTARGET%};
  CAppInfo = CAppName + ' ' + CAppVersion + ' ' + CBuild + ' ' + CCompiler;
