import React from "react";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import { ContactPage } from "./Pages/ContactPage";
import { FindsChristmasWordsPage } from "./Pages/FindChristmasWordsPage";
import { WelcomePage } from "./Pages/WelcomePage";

function App() {
  return (
    <Router>
      <Switch>
        <Route exact path="/" component={WelcomePage} />
        <Route exact path="/welcome" component={WelcomePage} />
        <Route exact path="/christmas" component={FindsChristmasWordsPage} />
        <Route exact path="/contact" component={ContactPage} />
      </Switch>
    </Router>
  );
}

export default App;
