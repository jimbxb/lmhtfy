import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.copy && app.ports.copy.subscribe((id) => {
  // https://stackoverflow.com/questions/36639681/how-to-copy-text-from-a-div-to-clipboard
  const elt = document.getElementById(id);
  if (window.getSelection) {
    if (window.getSelection().empty) { // Chrome
      window.getSelection().empty();
    } else if (window.getSelection().removeAllRanges) { // Firefox
      window.getSelection().removeAllRanges();
    }
  } else if (document.selection) { // IE?
    document.selection.empty();
  }

  if (document.selection) {
    var range = document.body.createTextRange();
    range.moveToElementText(elt);
    range.select().createTextRange();
    document.execCommand("copy");
  } else if (window.getSelection) {
    var range = document.createRange();
    range.selectNode(elt);
    window.getSelection().addRange(range);
    document.execCommand("copy");
    if (window.getSelection().empty) { // Chrome
      window.getSelection().empty();
    } else if (window.getSelection().removeAllRanges) { // Firefox
      window.getSelection().removeAllRanges();
    }
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
