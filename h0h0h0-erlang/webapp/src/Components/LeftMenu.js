import { useHistory } from "react-router-dom";
import React from "react";
import { Menu, Grid } from "antd";
const SubMenu = Menu.SubMenu;

const { useBreakpoint } = Grid;

//Reference: https://github.com/Rupinderthind/Ant_design_navbar/blob/master/src/App.js
const LeftMenu = () => {
  const { md } = useBreakpoint();
  const history = useHistory();
  return (
    <Menu mode={md ? "horizontal" : "inline"}>
      <Menu.Item key="mail">
        <a href="/welcome">Home</a>
      </Menu.Item>
      <SubMenu key="sub1" title={<span>Text Transformation</span>}>
        <Menu.Item key="setting:4" onClick={() => history.push("/christmas")}>
          Find Christmas words
        </Menu.Item>
      </SubMenu>
      <Menu.Item key="contact">
        <a href="/contact">Contact us</a>
      </Menu.Item>
    </Menu>
  );
};

export default LeftMenu;
