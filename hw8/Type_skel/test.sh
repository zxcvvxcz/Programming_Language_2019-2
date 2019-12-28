#!/bin/bash
ERROR_MSG_ANS="typeerror"
ERROR_MSG_RUN="TypeCheckingFailed"
for f in ./examples/*.m; do
  answer=$(tail -n 1 "$f")
  tmp=${answer#*: }
  tmp=${tmp%\**}
  ANSWER="${tmp// /}"

  tmp="$(./run "$f")"
  OUTPUT="${tmp// /}"

  if [ "$ANSWER" = "$OUTPUT" ]; then
    echo $f : O;
  elif [ "$ANSWER" = $ERROR_MSG_ANS ] && [ "$OUTPUT" = $ERROR_MSG_RUN ]; then
    echo $f : O;
  else
    echo $(head -n 1 "$f")
    echo ${f##*/} : "Expected :" $ANSWER "Got :" $OUTPUT
    echo
  fi
done