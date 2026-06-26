/**
 *
bubbles: false
cancelBubble: false
cancelable: false
code: 1000
composed: false
currentTarget: null
defaultPrevented: false
eventPhase: 0
explicitOriginalTarget: WebSocket { url: "ws://localhost:5000/ws/01KNW48VB0PNCFC0KZ8SW289ZV", readyState: 3, bufferedAmount: 0, … }
isTrusted: true
originalTarget: WebSocket { url: "ws://localhost:5000/ws/01KNW48VB0PNCFC0KZ8SW289ZV", readyState: 3, bufferedAmount: 0, … }
reason: ""
returnValue: true
srcElement: WebSocket { url: "ws://localhost:5000/ws/01KNW48VB0PNCFC0KZ8SW289ZV", readyState: 3, bufferedAmount: 0, … }
target: WebSocket { url: "ws://localhost:5000/ws/01KNW48VB0PNCFC0KZ8SW289ZV", readyState: 3, bufferedAmount: 0, … }
timeStamp: 313239
type: "close"
wasClean: true
 */
export const _fromEventImpl = (evt) => {
  if (
    evt?.type === 'close' &&
    "reason" in evt &&
    typeof evt.reason === 'string' &&
      "code" in evt &&
      typeof evt.code === 'number'
  ) {
    return evt;
  }

  return null;
}

