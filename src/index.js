import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const clearSelection = () => {
  if (window.getSelection) {
    if (window.getSelection().empty) { // Chrome
      window.getSelection().empty();
    } else if (window.getSelection().removeAllRanges) { // Firefox
      window.getSelection().removeAllRanges();
    }
  } else if (document.selection) { // IE?
    document.selection.empty();
  }
}

app.ports.copy && app.ports.copy.subscribe((id) => {
  const elt = document.getElementById(id);

  clearSelection();
  if (document.selection) {
    var range = document.body.createTextRange();
    range.moveToElementText(elt);
    range.select().createTextRange();
  } else if (window.getSelection) {
    var range = document.createRange();
    range.selectNode(elt);
    window.getSelection().addRange(range);
  }
  document.execCommand("copy");
  clearSelection();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
