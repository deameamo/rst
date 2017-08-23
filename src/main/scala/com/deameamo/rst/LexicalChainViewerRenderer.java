package com.deameamo.rst;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JList;

public class LexicalChainViewerRenderer extends DefaultListCellRenderer {
	private static final long serialVersionUID = 1L;
	
	private ImageIcon icon = new ImageIcon("icons/chain.png");
	
	public Component getListCellRendererComponent(
        JList<?> list,
        Object value,
        int index,
        boolean isSelected,
        boolean cellHasFocus)
    {
		super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
		setIcon(icon);
		return this;
    }

}
