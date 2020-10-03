import './main.css';
import { Elm } from './App.elm';
import landingImage from './Resources/landing.png';
import registerServiceWorker from './registerServiceWorker';

// key for session cache in localstorage
var storageKey = "session";

// hooks into and modifies jsonString to add required resources
// used when loading the initial cache object from localstorage
function addResources(jsonString) {
  // Parse the object
  var jsonObject = JSON.parse(jsonString);

  // Protect against a null object
  if (jsonObject == null) {
    jsonObject = { "auth": null };
  };

  // Add resources
  jsonObject["landingImage"] = landingImage;

  // Return the stringified object
  return JSON.stringify(jsonObject);
}

// app entry point
var app = Elm.App.init({
  node: document.getElementById('root'),
  flags: addResources(localStorage.getItem(storageKey))
});

// when the cache msg is sent, store the session data into local storage
app.ports.cache.subscribe(function (data) {
  localStorage.setItem(storageKey, JSON.stringify(data))
})

// OBJECTIVE NOTE POST-PROCESSING
// This isn't very safe and definitely not ideal. 
// It post-processes objective notes with:
// - mermaid diagrams for all elements with class "lang-mermaid"
// - h1 conversion to details and summary (collapsible headings)
// Unfortunately, it's prone to error (poor interactions with Elm's Virtal DOM?)
// Need to think if there's a more idiomatic way to do this. Otherwise, must wrap
// in a try-catch block to prevent crashing the whole app when it fails.

// external package mermaid for diagrams
var mermaid = require("mermaid");

// initialize mermaid
mermaid.mermaidAPI.initialize({
  startOnLoad: false,
  logLevel: "fatal",
});


// global variable used to auto-generate unique IDs for mermaid
// if integer overflow occurs, then reset the count and add a character to the prefix. 
// not ideal, but works for now.
var count = 0;
var prefix = "";
// generate a new unique ID prefixed with mermaid and mutate the ID template variables
function generateMermaidId() {
  var newId = "mermaid-" + prefix + String(count);
  if (count == Number.MAX_SAFE_INTEGER) {
    count = 0;
    prefix += "x";
  } else {
    count += 1;
  };
  return newId;
};

// checks for all elements with class lang-mermaid and renders mermaid
function renderMermaid() {
  var elements = document.getElementsByClassName("lang-mermaid");
  Array.prototype.forEach.call(elements, element => {
    var newId = generateMermaidId();
    try {
      var insertSvg = (svgCode) => { element.innerHTML = svgCode; element.className = "diagram-mermaid"; element.parentElement.className = "container-mermaid"; };
      var graph = mermaid.mermaidAPI.render(newId, element.textContent, insertSvg);
    } catch (e) {
      // remove the syntax error element...there should be a better way to do this i.e. not generating the error element in the first place!
      document.getElementById("d" + newId).remove();
    };
  });
};

// processes all h1 children in markdown child of #objective-body-notes into collapsible details/summary
function makeCollapsibleH1() {

  // get the first (and only) child of the objective-body-notes element (this is the markdown container)
  var container = document.getElementById("objective-body-notes").children[0];
  // get all child elements of this container i.e. all rendered markdown tags, and its parent container
  var containerParent = container.parentElement;

  // a new element which will be used to replace the current markdown container
  var newContainer = document.createElement("div");
  newContainer.className = "markdown";

  // a function that will take an object with props "header" and "children" and push it into newElement's children
  function pushSection(section) {
    // create new elements
    var newDetails = document.createElement("details");
    newDetails.className = "collapsible-heading";
    var newSummary = document.createElement("summary");
    // if header is entitled "Summary", then default to open
    if (section.header.textContent == "Summary") { newDetails.open = true; };
    // add data to elements
    newSummary.appendChild(section.header);
    newDetails.appendChild(newSummary);
    // add all the children of the section into the new details element
    for (var j = 0; j < section.children.length; j++) {
      newDetails.appendChild(section.children[j]);
    };
    // append the details element into the new container element
    newContainer.appendChild(newDetails);
  };

  // mutable store for any content coming before the first h1 element
  var initial = [];
  // the current section
  var currentSection = {};
  // whether the state is currently inside a section (i.e. has the first h1 been encountered?)
  var inSection = false;
  // iterate through all rendered markdown elements and divide into sections
  while (container.children.length > 0) {
    var child = container.removeChild(container.children[0]);
    if (child.tagName == "H1") {
      // when a new H1 section is encountered, process and push the currentSection
      if (inSection) {
        pushSection(currentSection);
        currentSection = { header: child, children: [] };
      } else {
        inSection = true;
        currentSection = { header: child, children: [] };
      }
    } else {
      if (inSection) { currentSection.children.push(child); }
      else { initial.push(child); }
    };
  };
  // push the last currentSection if any H1 was encountered
  if (inSection) {
    pushSection(currentSection);
  };
  // insert the initial data
  var initialElement = document.createElement("div");
  initialElement.className = "initial-content";
  for (var i = 0; i < initial.length; i++) { initialElement.appendChild(initial[i]) };
  newContainer.insertBefore(initialElement, newContainer.childNodes[0]);

  // replace the container with the newContainer
  containerParent.innerHTML = "";
  containerParent.appendChild(newContainer);
  // remove loading-hidden from className of parent
  containerParent.className = "objective-body-notes markdown"
};

// perform all post-processing functions when viewing an objective
app.ports.postProcessObjectiveView.subscribe(function () {
  setTimeout(() => { makeCollapsibleH1(); renderMermaid(); }, 50);
})

// perform a subset of post-processing functions when editing an objective
app.ports.postProcessObjectiveEdit.subscribe(function () {
  setTimeout(() => { renderMermaid(); }, 50);
})

// perform a subset of post-processing functions when view an objective from a question modal
app.ports.postProcessObjectiveModal.subscribe(function () {
  setTimeout(() => { makeCollapsibleH1(); renderMermaid() }, 50);
})

registerServiceWorker(app);
