package com.mediarithmics;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "UserGroup")
public class Group {

    @Id
    @SequenceGenerator(name = "group_id_seq", sequenceName="group_id_seq", allocationSize=1)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "group_id_seq")
    private long id;

    @ManyToMany(cascade = CascadeType.ALL)
    @JoinTable(
            name = "user_groups",
            joinColumns = @JoinColumn(name = "group_id"),
            inverseJoinColumns = @JoinColumn(name = "user_id"))
    List<User> users;

    public Group(String name){
        this.name = name;
    }

    public Group(){
        this("");
    }

    @Column(nullable = false)
    private String name;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Iterable<User> getUsers(){
        return users;
    }

    public void addUser(User u){
        this.users.add(u);
        u.groups.add(this);
    }

    public void removeUrer(User u){
        u.groups.remove(this);
        this.users.remove(u);
    }
}