import React, { Component } from "react";
import LeftMenu from "./LeftMenu";
import { Drawer, Button } from "antd";
import logo from "../erlang-logo.png";
import "./NavBar.css";

//Reference: https://github.com/Rupinderthind/Ant_design_navbar/blob/master/src/App.js
class Navbar extends Component {
  state = {
    current: "mail",
    visible: false,
  };
  showDrawer = () => {
    this.setState({
      visible: true,
    });
  };

  onClose = () => {
    this.setState({
      visible: false,
    });
  };

  render() {
    return (
      <nav className="menuBar">
        <div className="logo">
          <a href="/">
            <img src={logo} alt="Logo" />
          </a>
        </div>
        <div className="menuCon">
          <div className="leftMenu">
            <LeftMenu />
          </div>
          <Button className="barsMenu" type="primary" onClick={this.showDrawer}>
            <span className="barsBtn"></span>
          </Button>
          <Drawer
            title="Basic Drawer"
            placement="right"
            closable={false}
            onClose={this.onClose}
            visible={this.state.visible}
          >
            <LeftMenu />
          </Drawer>
        </div>
      </nav>
    );
  }
}

export default Navbar;
