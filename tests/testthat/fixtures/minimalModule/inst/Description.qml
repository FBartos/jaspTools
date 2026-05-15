import QtQuick
import JASP.Module

Description
{
	title: qsTr("jaspTools Syntax Test Module")
	description: qsTr("Minimal module fixture for jaspTools bridge tests.")
	preloadData: true
	hasWrappers: true

	Analysis
	{
		title: qsTr("Minimal Analysis")
		func: "MinimalAnalysis"
		qml: "MinimalAnalysis.qml"
		preloadData: false
	}
}
