import QtQuick
import JASP
import JASP.Controls

Form
{
	CheckBox
	{
		name: "flag"
		label: qsTr("Flag")
		checked: true
	}

	DoubleField
	{
		name: "threshold"
		label: qsTr("Threshold")
		defaultValue: 1.5
	}

	RadioButtonGroup
	{
		name: "choice"
		title: qsTr("Choice")

		RadioButton
		{
			value: "one"
			label: qsTr("One")
		}

		RadioButton
		{
			value: "two"
			label: qsTr("Two")
			checked: true
		}
	}
}
