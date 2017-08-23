package com.deameamo.rst;

import java.awt.Component;

import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JList;

public class FileListRenderer extends DefaultListCellRenderer {
    private static final long serialVersionUID = 1L;

    private ImageIcon completeIcon = new ImageIcon("icons/document.png");
    private ImageIcon emptyIcon = new ImageIcon("icons/document_empty.png");
    private ImageIcon incompleteIcon = new ImageIcon("icons/document_warning.png");

    public Component getListCellRendererComponent(
            JList<?> list,
            Object value,
            int index,
            boolean isSelected,
            boolean cellHasFocus)
    {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        RSTTreeModel model = Resource.getTreeModel(value.toString());
        if(model.isEmpty())
            setIcon(emptyIcon);
        else if(model.isComplete() && model.isLegal())
            setIcon(completeIcon);
        else
            setIcon(incompleteIcon);
        return this;
    }
}