; ModuleID = 'ekitai_module'
source_filename = "ekitai_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%Option = type { i1, %"Option::Some" }
%"Option::Some" = type { i64 }

define %Option @match_option(%Option %input) {
  %1 = extractvalue %Option %input, 0
  switch i1 %1, label %case.else [
    i1 false, label %br.0.tag
    i1 true, label %br.1.tag
  ]

case.else:                                        ; preds = %0
  unreachable

br.0.tag:                                         ; preds = %0
  br label %case.merge

br.1.tag:                                         ; preds = %0
  %2 = extractvalue %Option %input, 1
  %3 = extractvalue %"Option::Some" %2, 0
  br label %case.merge

case.merge:                                       ; preds = %br.1.tag, %br.0.tag
  %phi = phi %Option [ %input, %br.0.tag ], [ { i1 true, %"Option::Some" zeroinitializer }, %br.1.tag ]
  ret %Option %phi
}
